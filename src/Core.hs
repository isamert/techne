{-# LANGUAGE DeriveDataTypeable         #-}
module Core where

import TechnePrelude
import Err
import Syntax
import Parser

import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Control.Monad.Identity
import Control.Monad.Except

-- ----------------------------------------------------------------------------
-- Core definitions
-- ----------------------------------------------------------------------------

type CoreM a = TechneM Identity a

data CVal
    = CDat Name [CExpr]
    | CLit Lit
    deriving (Show, Eq, Typeable, Data)

data Closure
    = UClosure Name CExpr Env
    | PClosure Name Int [CExpr]
    deriving (Show, Eq, Typeable, Data)

data CExpr
    = CRef   Name
    | CVal   CVal
    | CLam   Name CExpr
    | CApp   CExpr CExpr
    | CMatch CExpr [(Pattern, CExpr)]
    | CFix   CExpr
    | Closure Closure
    deriving (Show, Eq, Typeable, Data)

type Env = Map.Map Text CExpr

pattern CaseTrue  = UnpackPattern Nothing "True"  (Tuple [])
pattern CaseFalse = UnpackPattern Nothing "False" (Tuple [])

-- ----------------------------------------------------------------------------
-- Core
-- ----------------------------------------------------------------------------

-- FIXME: compile patterns
coreExpr :: Expr -> CoreM CExpr
coreExpr (LitExpr lit) = return $ CVal (CLit lit)
coreExpr (FnApplExpr expr (Tuple xs)) = do
    e <- coreExpr expr
    ctup <- tuple2ctuple xs
    return $ foldl CApp e ctup
coreExpr (BinExpr op l r) = do
    exprs <- mapM coreExpr [l,r]
    return $ foldl CApp (CRef op) exprs
coreExpr (UnExpr op expr) = CApp (CRef op) <$> coreExpr expr
coreExpr (RefExpr (Ref name)) = return $ CRef name
coreExpr (RefExpr (PlaceHolder _)) = throwError $ CoreErr DesugaringError
coreExpr (EFn name prms body _) = do
    e <- coreExpr body
    cprms <- mapM param2name prms
    return $ foldr CLam e cprms
coreExpr (WhenExpr cases) = cases2match cases
coreExpr (MatchExpr expr cases) = do
    e <- coreExpr expr
    cs <- mapM (mapM coreExpr) cases
    return $ CMatch e cs
coreExpr (FixExpr xs) = CFix <$> coreExpr xs
coreExpr (EList xs) = list2clist xs
coreExpr (ETuple xs) = do
    ctuple <- tuple2ctuple xs
    return $ foldl CApp (CRef tupleConstructor) ctuple
    where tupleConstructor = "Tuple" ++ (tshow . length $ fixTupleOrder xs)

-- FIXME: ImplDecl's
-- FIXME: imports
coreModule :: Module -> CoreM Env
coreModule (Module imports decls) = do
    cdecls <- mapM coreDecl decls
    return $ foldl Map.union Map.empty cdecls

coreDecl :: Decl -> CoreM Env
coreDecl (FnDecl fn@(Fn (Just name) prms body scope)) = do
    cfn <- coreExpr $ FnExpr fn
    cscp <- mapM coreDecl scope
    return $ Map.union (Map.singleton name cfn)
              (foldl Map.union Map.empty cscp)
coreDecl (DataDecl (Dat _ _ constrs)) = return $ Map.fromList $ map constr2fn constrs

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

tuple2ctuple :: [TupleElem Expr] -> CoreM [CExpr]
tuple2ctuple xs = mapM coreExpr $ fixTupleOrder xs

param2name :: Param -> CoreM Name
param2name (Param (BindPattern name _)) = return name
param2name (Param ptrn) = throwError $ CoreErr DesugaringError

cases2match :: [(Expr, Expr)] -> CoreM CExpr
cases2match [] = return $ CApp (CRef "error") $ CVal . CLit $ StrLit "unhandled case in when expr"
cases2match ((el, er):cs) = do
    cel <- coreExpr el
    cer <- coreExpr er
    ccs <- cases2match cs
    return $ CMatch cel [(CaseTrue,  cer)
                        ,(CaseFalse, ccs)]

list2clist :: [Expr] -> CoreM CExpr
list2clist [] = return $ CRef "Nil"
list2clist (x:xs) = do
    cx <- coreExpr x
    cxs <- list2clist xs
    return $ foldl CApp (CRef "Cons") [cx, cxs]

constr2fn :: (Name, [DataParam]) -> (Name, CExpr)
constr2fn (name, xs) = (name, foldr CLam (CVal (CDat name refs)) params)
    where params = zipWith (\_ n -> "prm" ++ tshow n ++ name) xs [1..]
          refs   = map CRef params

runCore :: CoreM a -> TechneResult a
runCore m = runIdentity (runExceptT $ m)
