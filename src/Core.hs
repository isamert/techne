{-# LANGUAGE DeriveDataTypeable         #-}
module Core where

import TechnePrelude
import Syntax
import Parser

import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import Data.Data (Data)

-- ----------------------------------------------------------------------------
-- Core definitions
-- ----------------------------------------------------------------------------

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
coreExpr :: Expr -> CExpr
coreExpr (LitExpr lit) = CVal (CLit lit)
coreExpr (FnApplExpr expr (Tuple xs)) = foldl CApp (coreExpr expr) (tuple2ctuple xs)
coreExpr (BinExpr op l r) = foldl CApp (CRef op) (map coreExpr [l,r])
coreExpr (UnExpr op expr) = CApp (CRef op) (coreExpr expr)
coreExpr (RefExpr (Ref name)) = CRef name
coreExpr (RefExpr (PlaceHolder _)) = desugarErr
coreExpr (EFn name prms body _) = foldr CLam (coreExpr body) (map param2name prms)
coreExpr (WhenExpr cases) = cases2match cases
coreExpr (MatchExpr expr cases) = CMatch (coreExpr expr) (map (map coreExpr) cases)
coreExpr (FixExpr xs) = CFix (coreExpr xs)
coreExpr (EList xs) = list2clist xs
coreExpr (ETuple xs) = foldl CApp (CRef tupleConstructor) (tuple2ctuple xs)
    where tupleConstructor = "Tuple" ++ (tshow . length $ fixTupleOrder xs)

-- FIXME: ImplDecl's
-- FIXME: imports
coreModule :: Module -> Env
coreModule (Module imports decls) = foldl Map.union Map.empty $ map coreDecl decls

coreDecl :: Decl -> Env
coreDecl (FnDecl fn@(Fn (Just name) prms body scope)) =
    Map.union (Map.singleton name (coreExpr $ FnExpr fn))
              (foldl Map.union Map.empty $ map coreDecl scope)
coreDecl (DataDecl (Dat _ _ constrs)) = Map.fromList $ map constr2fn constrs

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

desugarErr :: a
desugarErr = error "Something went wrong in desugaring phase."

tuple2ctuple :: [TupleElem Expr] -> [CExpr]
tuple2ctuple xs = map coreExpr $ fixTupleOrder xs

param2name :: Param -> Name
param2name (Param (BindPattern name _)) = name
param2name (Param ptrn) = desugarErr

cases2match :: [(Expr, Expr)] -> CExpr
cases2match [] = CApp (CRef "error") $ CVal . CLit $ StrLit "unhandled case in when expr"
cases2match ((el, er):cs) = CMatch (coreExpr el) [(CaseTrue,  coreExpr er)
                                                 ,(CaseFalse, cases2match cs)]

list2clist :: [Expr] -> CExpr
list2clist [] = CRef "Nil"
list2clist (x:xs) = foldl CApp (CRef "Cons") [coreExpr x, list2clist xs]

constr2fn :: (Name, [DataParam]) -> (Name, CExpr)
constr2fn (name, xs) = (name, foldr CLam (CVal (CDat name refs)) params)
    where params = zipWith (\_ n -> "prm" ++ tshow n ++ name) xs [1..]
          refs   = map CRef params
