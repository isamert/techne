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
type GenEnv = Map.Map Name GenName  -- a map from names to generated names

data RState = RState { counter :: Int, currEnv :: GenEnv }
newtype RError = NotAssigned Text deriving (Show, Eq)
type RenamerM a = ExceptT RError (State RState) a

-- TODO: seems so repetetive but I have no idea about solving it, uniplate
-- is not helping I believe.

-- NamedTElems are not renamed because there is no way to know which data type
-- they belong to (or even we can't know if they belong to a data type or not).
-- But (hopefully) this is not a problem because they are accessed trough a
-- variable all the time.
-- Also data type names are not renamed, because if there is a name shadowing
-- that should be reported.

-- ----------------------------------------------------------------------------
-- Toplvl renamers
-- ----------------------------------------------------------------------------
renameModule :: Module -> GenEnv -> RenamerM Module
renameModule (Module imports decls) env = do
    gdecls <- resetCurrEnv >> mapM renameFnNames decls
    currenv <- gets currEnv
    let newenv = Map.union currenv env -- Maybe check conflicts here before union (with other modules)
    Module imports <$> mapM (`renameFns` env) gdecls
    where renameFnNames (FnDecl fn@Fn {fnName=(Just name)})  = do
            gname <- insertCurrEnv name
            return $ FnDecl $ fn { fnName = Just gname }
          renameFns (FnDecl fn@Fn { fnBody = body }) env = do
              b <- renameExpr body env
              return $ FnDecl fn { fnBody = b }

-- ----------------------------------------------------------------------------
-- renameExpr
-- ----------------------------------------------------------------------------
-- TODO: rename PlaceHolders too
renameExpr :: Expr -> GenEnv -> RenamerM Expr
renameExpr (RefExpr ref@Ref {refName=name}) env = do
    name <- renameFreeVar name env
    return $ RefExpr ref {refName=name}

renameExpr (ListExpr list) env = ListExpr <$> renameList (`renameExpr` env) list
renameExpr (TupleExpr tuple) env = TupleExpr <$> renameTuple (`renameExpr` env) tuple

-- TODO: scope
renameExpr (FnExpr fn@Fn {fnParams = params, fnBody = expr, fnScope = scope}) env = do
    prms <- resetCurrEnv >> renameParams
    currenv <- gets currEnv
    exp  <- renameExpr expr (Map.union currenv env) -- union is left-biased
    return $ FnExpr $ fn { fnParams = prms, fnBody = exp }
    where renameParams = forM params $ \case
            (DataParam name typ) -> flip DataParam typ <$> insertCurrEnv name
            (Param ptrn typ) -> flip Param typ <$> renamePattern ptrn

renameExpr (FnApplExpr name tuple) env = liftM2 FnApplExpr  (renameFreeVar name env) $ renameTuple (`renameExpr` env) tuple

renameExpr (MatchExpr test cases) env = do
    t  <- renameExpr test env
    cs <- mapM renameCase cases
    return $ MatchExpr t cs
    where renameCase (pttrn, expr) = do
            p <- resetCurrEnv >> renamePattern pttrn
            currenv <- gets currEnv
            e <- renameExpr expr (Map.union currenv env)
            return (p, e)

renameExpr (WhenExpr test cases) env = do
    t  <- mapM (`renameExpr` env) test
    cs <- mapM renameCase cases
    return $ WhenExpr t cs
    where renameCase (expr1, expr2) = do
            e1 <- renameExpr expr1 env
            e2 <- renameExpr expr2 env
            return (e1, e2)

renameExpr (BinExpr op right left) env = liftM3 BinExpr (renameBinOp op env) (renameExpr right env) (renameExpr left env)
    where renameBinOp (Op name) env = Op <$> renameFreeVar name env
          renameBinOp op        _   = return op
renameExpr x _ = return x

-- ----------------------------------------------------------------------------
-- Individual renamers
-- ----------------------------------------------------------------------------
renameFreeVar :: Name -> GenEnv -> RenamerM GenName
renameFreeVar name env =
    case Map.lookup name env of
      Just gname -> return gname
      Nothing    -> throwError (NotAssigned name)

renamePattern :: Pattern -> RenamerM Pattern
renamePattern (BindPattern name) = BindPattern <$> insertCurrEnv name
renamePattern (UnpackPattern name dataname tuple) = UnpackPattern name dataname <$> renameTuple renamePattern tuple
renamePattern ptrn = case ptrnName ptrn of
                     Just name -> insertCurrEnv name >>= \genname -> return $ ptrn { ptrnName = Just genname }
                     Nothing -> return ptrn

renameTuple :: (a -> RenamerM a) -> Tuple a -> RenamerM (Tuple a)
renameTuple f (Tuple xs) =
    Tuple <$> mapM renameTElem xs
    where renameTElem (IndexedTElem elem)    = IndexedTElem <$> f elem
          renameTElem (NamedTElem name elem) = NamedTElem name <$> f elem

renameList :: (a -> RenamerM a) -> List a -> RenamerM (List a)
renameList f (List xs) = List <$> mapM f xs

-- ----------------------------------------------------------------------------
-- RenamerM utility
-- ----------------------------------------------------------------------------
initRState :: RState
initRState = RState { counter = 0, currEnv = Map.empty }

runRenamer :: RenamerM a -> Either RError a
runRenamer m = evalState (runExceptT m) initRState

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
