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

data CPattern
   = CPBind   Name
   | CPElse   (Maybe Name)
   | CPLit    (Maybe Name) Lit
   | CPRegex  (Maybe Name) Text
   | CPUnpack (Maybe Name) Name [CPattern]
    deriving (Show, Eq, Typeable, Data)

data Closure
    = UClosure Name CExpr Env
    | PClosure Name Int [CExpr]
    deriving (Show, Eq, Typeable, Data)

data CExpr
    = CRef    Name
    | CVal    CVal
    | CLam    Name CExpr
    | CApp    CExpr CExpr
    | CMatch  CExpr [(CPattern, CExpr)]
    | CFix    CExpr
    | Closure Closure
    deriving (Show, Eq, Typeable, Data)

type Env = Map.Map Text CExpr

pattern CaseTrue        = CPUnpack Nothing "True"  []
pattern CaseFalse       = CPUnpack Nothing "False" []
pattern CaseCons x rest = CPUnpack Nothing "Cons"  [x, rest]
pattern CaseNil         = CPUnpack Nothing "Nil"   []

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
    cs <- mapM toCMatchCase cases
    return $ CMatch e cs
    where toCMatchCase (p,c) = do
            cptrn <- ptrn2cptrn p
            cexpr <- coreExpr c
            return $ (cptrn, cexpr)
coreExpr (FixExpr xs) = CFix <$> coreExpr xs
coreExpr (EList xs) = list2clist xs
coreExpr (ETuple xs) = do
    ctuple <- tuple2ctuple xs
    return $ foldl CApp (CRef $ tupleConstructorName xs) ctuple
    where

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
tupleConstructorName  :: [TupleElem a] -> Name
tupleConstructorName xs = "Tuple" ++ (tshow . length $ fixTupleOrder xs)

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

ptrn2cptrn :: Pattern -> CoreM CPattern
ptrn2cptrn (BindPattern name _) = return $ CPBind name
ptrn2cptrn (ElsePattern name)   = return $ CPElse name
ptrn2cptrn (LitPattern name lit) = return $  CPLit name lit
ptrn2cptrn (RegexPattern name regex) = return $ CPRegex name regex
ptrn2cptrn (ListPattern name (List list)) = do
    clist <- mapM ptrn2cptrn list
    case lastSafe list of
      Just (RestPattern _) -> return $ foldr1 CaseCons clist
      Just _               -> return $ foldr CaseCons CaseNil clist
      Nothing              -> return $ CaseNil

ptrn2cptrn (RestPattern name) = return $ case name of
                                  Just name -> CPBind name
                                  Nothing   -> CPElse Nothing
ptrn2cptrn (TuplePattern name (Tuple telems)) = do
    ctelems <- mapM ptrn2cptrn $ fixTupleOrder telems
    return $ CPUnpack name (tupleConstructorName telems) ctelems
ptrn2cptrn (UnpackPattern name dname (Tuple telems)) =
    CPUnpack name dname <$> mapM ptrn2cptrn (fixTupleOrder telems)

constr2fn :: (Name, [DataParam]) -> (Name, CExpr)
constr2fn (name, xs) = (name, foldr CLam (CVal (CDat name refs)) params)
    where params = zipWith (\_ n -> "prm" ++ tshow n ++ name) xs [1..]
          refs   = map CRef params

runCore :: CoreM a -> TechneResult a
runCore m = runIdentity (runExceptT $ m)
