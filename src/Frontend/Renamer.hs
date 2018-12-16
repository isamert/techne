module Frontend.Renamer where

import TechnePrelude
import Frontend.Syntax
import Frontend.Parser

import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

type GenName = Text                 -- a generated name
type GenEnv  = Map.Map Name GenName -- a map from names to generated names

data RenamerS    = RenamerS { counter :: Int, currEnv :: GenEnv }
newtype RenamerE = NotAssigned Text deriving (Show, Eq)
type RenamerM a  = ExceptT RenamerE (State RenamerS) a

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
renameExpr (ERef name typ) env = do
    name <- renameFreeVar name env
    return $ ERef name typ

renameExpr (ListExpr list) env = ListExpr <$> renameList (`renameExpr` env) list
renameExpr (TupleExpr tuple) env = TupleExpr <$> renameTuple (`renameExpr` env) tuple

-- TODO: scope
renameExpr fn@(EFn name prms_ rt expr_ scope) env = do
    prms <- resetCurrEnv >> renameParams
    currenv <- gets currEnv
    expr <- renameExpr expr_ (Map.union currenv env) -- union is left-biased
    return $ EFn name prms rt expr scope
    where renameParams = forM prms_ $ \case
            (Param ptrn typ) -> flip Param typ <$> renamePattern ptrn

renameExpr (FnApplExpr expr tuple) env =
    liftM2 FnApplExpr
           (renameExpr expr env) $ renameTuple (`renameExpr` env) tuple

renameExpr (MatchExpr test cases) env = do
    t  <- renameExpr test env
    cs <- mapM renameCase cases
    return $ MatchExpr t cs
    where renameCase (pttrn, expr) = do
            p <- resetCurrEnv >> renamePattern pttrn
            currenv <- gets currEnv
            e <- renameExpr expr (Map.union currenv env)
            return (p, e)

renameExpr (WhenExpr cases) env = do
    cs <- mapM renameCase cases
    return $ WhenExpr cs
    where renameCase (expr1, expr2) = do
            e1 <- renameExpr expr1 env
            e2 <- renameExpr expr2 env
            return (e1, e2)

renameExpr (BinExpr op right left) env = liftM3 BinExpr
                                                (renameOp op env)
                                                (renameExpr right env)
                                                (renameExpr left env)
renameExpr (UnExpr op operand) env = liftM2 UnExpr
                                            (renameOp op env)
                                            (renameExpr operand env)
renameExpr x _ = return x

-- ----------------------------------------------------------------------------
-- Individual renamers
-- ----------------------------------------------------------------------------

renameFreeVar :: Name -> GenEnv -> RenamerM GenName
renameFreeVar name env =
    case Map.lookup name env of
      Just gname -> return gname
      Nothing    -> throwError (NotAssigned name)

renameOp :: Op -> GenEnv -> RenamerM Op
renameOp (BinOp name) env = BinOp <$> renameFreeVar name env
renameOp (UnOp  name) env = UnOp <$> renameFreeVar name env

renamePattern :: Pattern -> RenamerM Pattern
renamePattern (BindPattern name) = BindPattern <$> insertCurrEnv name
renamePattern (UnpackPattern name dataname tuple) =
    UnpackPattern name dataname <$> renameTuple renamePattern tuple
renamePattern ptrn =
    case ptrnName ptrn of
      Just name -> insertCurrEnv name >>= \genname -> return $ ptrn { ptrnName = Just genname }
      Nothing   -> return ptrn

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

initRenamerS :: RenamerS
initRenamerS = RenamerS { counter = 0, currEnv = Map.empty }

runRenamer :: RenamerM a -> Either RenamerE a
runRenamer m = evalState (runExceptT m) initRenamerS

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
