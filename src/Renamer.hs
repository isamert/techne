module Renamer
    ( RenamerM(..)
    , RenamerS(..)
    , GenEnv(..)
    , GenName(..)
    , renameModule
    , renameExpr
    , renameDecls
    -- utility
    , initRenamerS
    , emptyGenEnv
    , evalRenamer'
    , runRenamer
    , runRenamer'
    , runRenamerWithoutErr
    ) where

import TechnePrelude
import Err
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

type RenamerM a  = TechneM (State RenamerS) a

-- FIXME: currEnv is stupid. Explicitly return a generated env.
-- FIXME: name clashes between modules are not reported.
-- FIXME: probably need to return GenEnv to REPL

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

-- FIXME: main function (rename it to PROGRAM_ENTRY or smth like that)
renameDecls :: [Decl] -> GenEnv -> RenamerM [Decl]
renameDecls decls env = do
    gdecls <- resetCurrEnv >> mapM (`renameDeclNames` env) decls
    currenv <- gets currEnv
    let newenv = Map.union currenv env
    -- FIXME: ^^ Maybe check conflicts here before union (with other modules)
    mapM (`renameDecl` newenv) gdecls
        where renameDeclNames (FnDecl fn@Fn {fnName=(Just name)}) env  = do
                gname <- newTopLvlName name
                return $ FnDecl $ fn { fnName = Just gname }
              renameDeclNames decl env = return decl

              renameDecl (FnDecl fn@Fn{}) env = do
                b <- renameExpr (FnExpr fn) env
                return $ FnDecl (fnExprFn b)

              -- FIXME: rename ImplDecl, ConceptDecl
              renameDecl x env = return x

              newTopLvlName name@"main" = do
                let gname = "PROGRAM_ENTRY"
                s <- get
                put s { currEnv = Map.insert name gname (currEnv s) }
                return gname
              newTopLvlName name = insertCurrEnv name


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
                                                (renameFreeVar op env)
                                                (renameExpr right env)
                                                (renameExpr left env)

renameExpr (UnExpr op operand) env = liftM2 UnExpr
                                            (renameFreeVar op env)
                                            (renameExpr operand env)

renameExpr (FixExpr expr) env = FixExpr <$> renameExpr expr env

renameExpr x _ = return x

-- ----------------------------------------------------------------------------
-- Individual renamers
-- ----------------------------------------------------------------------------

renameFreeVar :: Name -> GenEnv -> RenamerM GenName
renameFreeVar name env =
    if  tisUpperFirst name || tisPrefixOf "internal" name
       then return name
       else case Map.lookup name env of
              Just gname -> return gname
              Nothing    -> do
                  fail <- gets allowFail
                  if fail
                     then throwError (RenamerErr $ NotAssigned name)
                     else genName

renamePattern :: Pattern -> RenamerM Pattern
renamePattern (BindPattern name typ) = flip BindPattern typ <$> insertCurrEnv name
renamePattern (ListPattern name list) = do
    gname <- insertCurrEnv' name
    ListPattern gname <$> renameList renamePattern list
renamePattern (TuplePattern name tuple) = do
    gname <- insertCurrEnv' name
    TuplePattern gname <$> renameTuple renamePattern tuple
renamePattern (UnpackPattern name dataname tuple) = do
    gname <- insertCurrEnv' name
    UnpackPattern gname dataname <$> renameTuple renamePattern tuple
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

-- FIXME: change evalRenamer to runRenamer and runRenamer to evalRenamer xdxd
evalRenamer' :: (a -> GenEnv -> RenamerM a) -> a -> GenEnv -> RenamerS -> (TechneResult a, RenamerS)
evalRenamer' m a env s = runState (runExceptT $ m a env) s

runRenamer :: Bool -> RenamerM a -> TechneResult a
runRenamer fail m = evalState (runExceptT m) $ initRenamerS fail

runRenamer' :: (a -> GenEnv -> RenamerM a) -> a -> TechneResult a
runRenamer' m a = evalState (runExceptT $ m a emptyGenEnv) $ initRenamerS True

runRenamerWithoutErr :: (a -> GenEnv -> RenamerM a) -> a -> a
runRenamerWithoutErr m a = fromRight' $
    evalState (runExceptT $ m a emptyGenEnv) $ initRenamerS False

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

insertCurrEnv' :: Maybe Name -> RenamerM (Maybe Name)
insertCurrEnv' name = case name of
                        Just x  -> Just <$> insertCurrEnv x
                        Nothing -> return Nothing

resetCurrEnv :: RenamerM ()
resetCurrEnv = do
    s <- get
    put s { currEnv = Map.empty }
    return ()

emptyGenEnv :: GenEnv
emptyGenEnv = Map.empty
