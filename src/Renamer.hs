module Renamer
    ( RenamerM(..)
    , renameModule
    , renameExpr
    , renameDecls
    -- utility
    , emptyGenEnv
    , runRenamer
    ) where

import TechnePrelude
import Syntax
import Parser

import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

type GenName = Text                 -- a generated name
type GenEnv  = Map.Map Name GenName -- a map from names to generated names

data RenamerS
    = RenamerS { counter   :: Int
               , currEnv   :: GenEnv
               , allowFail :: Bool
               } deriving (Show, Eq, Ord)

newtype RenamerE = NotAssigned Text deriving (Show, Eq, Ord)
type RenamerM a  = ExceptT RenamerE (State RenamerS) a

-- FIXME: currEnv is stupid. Explicitly return a generated env.
-- FIXME: name clashes between modules are not reported.

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
renameModule (Module impts decls) env = Module impts <$> renameDecls decls env

renameDecls :: [Decl] -> GenEnv -> RenamerM [Decl]
renameDecls decls env = do
    gdecls <- resetCurrEnv >> mapM (`renameDeclNames` env) decls
    currenv <- gets currEnv
    let newenv = Map.union currenv env
    -- FIXME: ^^ Maybe check conflicts here before union (with other modules)
    mapM (`renameDecl` newenv) gdecls
        where renameDeclNames (FnDecl fn@Fn {fnName=(Just name)}) env  = do
                gname <- insertCurrEnv name
                return $ FnDecl $ fn { fnName = Just gname }
              renameDeclNames decl env = return decl

              renameDecl (FnDecl fn@Fn{}) env = do
                b <- renameExpr (FnExpr fn) env
                return $ FnDecl (fnExprFn b)


-- ----------------------------------------------------------------------------
-- renameExpr
-- ----------------------------------------------------------------------------

renameExpr :: Expr -> GenEnv -> RenamerM Expr
renameExpr (ERef name) env = do
    name <- renameFreeVar name env
    return $ ERef name

renameExpr (ListExpr list) env = ListExpr <$> renameList (`renameExpr` env) list
renameExpr (TupleExpr tuple) env = TupleExpr <$> renameTuple (`renameExpr` env) tuple

renameExpr fn@(EFn name prms_ expr_ scope) env = do
    prms <- resetCurrEnv >> renameParams
    currenv <- gets currEnv
    expr <- renameExpr expr_ (Map.union currenv env) -- union is left-biased
    scop <- renameDecls scope env
    return $ EFn name prms expr scop
    where renameParams = forM prms_ $ \case
            (Param ptrn) -> Param <$> renamePattern ptrn

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

renameExpr (FixExpr expr) env = renameExpr expr env

renameExpr x _ = return x

-- ----------------------------------------------------------------------------
-- Individual renamers
-- ----------------------------------------------------------------------------

renameFreeVar :: Name -> GenEnv -> RenamerM GenName
renameFreeVar name env =
    case Map.lookup name env of
      Just gname -> return gname
      Nothing    -> do
          fail <- gets allowFail
          if fail
             then throwError (NotAssigned name)
             else genName

renameOp :: Op -> GenEnv -> RenamerM Op
renameOp (BinOp name) env = BinOp <$> renameFreeVar name env
renameOp (UnOp  name) env = UnOp <$> renameFreeVar name env

renamePattern :: Pattern -> RenamerM Pattern
renamePattern (BindPattern name typ) = flip BindPattern typ <$> insertCurrEnv name
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

runRenamer :: Bool -> RenamerM a -> Either RenamerE a
runRenamer fail m = evalState (runExceptT m) $ initRenamerS fail

initRenamerS :: Bool -> RenamerS
initRenamerS fail = RenamerS { counter = 0, currEnv = Map.empty, allowFail = fail }

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

emptyGenEnv :: GenEnv
emptyGenEnv = Map.empty
