{-# LANGUAGE TupleSections #-}
module Frontend.Renamer where

import TechnePrelude
import Frontend
import Frontend.AST
import Frontend.Parser

import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

type GenName = Text                 -- a generated name
type GenEnv = Map.Map Name GenName  --

data RState = RState { counter :: Int, currEnv :: GenEnv }
newtype RError = NotAssigned Text deriving (Show, Eq)
type RenamerM a = ExceptT RError (State RState) a

initRState :: RState
initRState = RState { counter = 0, currEnv = Map.empty }

runRenamer :: RenamerM a -> Either RError a
runRenamer m = evalState (runExceptT m) initRState

renameModule = undefined

-- TODO: rename PlaceHolders too
renameExpr :: Expr -> GenEnv -> RenamerM Expr
renameExpr (RefExpr ref@Ref {refName=name}) env =
    case Map.lookup name env of
      Just gname -> return $ RefExpr ref {refName = gname}
      Nothing    -> throwError (NotAssigned name)
renameExpr (ListExpr (List xs)) env =
    ListExpr . List <$> flip renameExpr env `mapM` xs
renameExpr (TupleExpr (Tuple xs)) env =
    TupleExpr . Tuple <$> renameTElem `mapM` xs
    where renameTElem (IndexedTElem expr)    = IndexedTElem <$> renameExpr expr env
          renameTElem (NamedTElem name expr) = NamedTElem name <$> renameExpr expr env

-- TODO: scope
-- FIXME: this seems just horrible
renameExpr (FnExpr fn@Fn {fnParams = params, fnBody = expr, fnScope = scope}) env = do
    prms <- resetCurrEnv >> renameParams
    currenv <- gets currEnv
    exp  <- renameExpr expr (Map.union currenv env) -- union is left-biased
    return $ FnExpr $ fn { fnParams = prms, fnBody = exp }
    where renameParams = forM params $ \case
            (DataParam name typ) -> flip DataParam typ <$> insertCurrEnv name
            (Param ptrn typ) -> flip Param typ <$> renamePattern ptrn
          renamePattern (BindPattern name) = BindPattern <$> insertCurrEnv name
          --FIXME: rename dataname
          renamePattern (UnpackPattern name dataname (Tuple ptrns)) = UnpackPattern name dataname <$> (Tuple <$> forM ptrns renameTElem)
          renamePattern ptrn = case ptrnName ptrn of
                                 Just name -> insertCurrEnv name >>= \genname -> return $ ptrn { ptrnName = Just genname }
                                 Nothing -> return ptrn
          renameTElem (IndexedTElem pttrn)    = IndexedTElem <$> renamePattern pttrn
          renameTElem (NamedTElem name pttrn) = NamedTElem name <$> renamePattern pttrn

-- TODO: look if it's a user defined one, otherwise etc
renameExpr BinExpr {binExprOp = op} env = undefined
renameExpr x _ = return x

genName :: RenamerM GenName
genName = do
    s <- get
    put s { counter = counter s + 1 }
    return $ "gname$" ++ tshow (counter s)

insertCurrEnv :: Name -> RenamerM GenName
insertCurrEnv name = do
    genname <- genName
    s <- get
    put s { currEnv = Map.insert name genname (currEnv s) }
    return genname

resetCurrEnv :: RenamerM ()
resetCurrEnv = do
    s <- get
    put s { currEnv = Map.empty }
    return ()
