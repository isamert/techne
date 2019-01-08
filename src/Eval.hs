module Eval where

import TechnePrelude
import Err
import Syntax
import Core

import Control.Monad.Identity
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Text.ICU as ICU

-- ----------------------------------------------------------------------------
-- Data definitions
-- ----------------------------------------------------------------------------

type InterpreterM a = TechneM Identity a

pattern CInt x = CVal (CLit (IntLit x))
pattern CFlt x = CVal (CLit (FltLit x))
type PEnv = Map.Map Text ([CExpr] -> CExpr)

primitiveEnv :: PEnv
primitiveEnv = Map.fromList $ map snd createEnv

defaultEnv :: Env
defaultEnv = Map.fromList $ map fst createEnv

createEnv :: [((Text, CExpr), (Text, [CExpr] -> CExpr))]
createEnv = [envElem "internalSum" 2 pPlus
            ,envElem "internalSub" 2 pMinus
            ,envElem "internalMul" 2 pMul
            ,envElem "internalDiv" 2 pDiv]

envElem :: Text -> Int -> b -> ((Text, CExpr), (Text, b))
envElem name n f = ((name, Closure (PClosure name n []))
                   ,(name, f))

pPlus  [CInt x, CInt y] = CInt (x + y)
pMinus [CInt x, CInt y] = CInt (x - y)
pMul   [CInt x, CInt y] = CInt (x * y)
pDiv   [CInt x, CInt y] = CFlt (fromIntegral x / fromIntegral y)

-- ----------------------------------------------------------------------------
-- Eval
-- ----------------------------------------------------------------------------

envLookup :: Env -> Name -> InterpreterM CExpr
envLookup env name =
    case Map.lookup name env of
      Just x -> return x
      Nothing -> throwError $ InterpreterErr RuntimeError

eval :: Env -> CExpr -> InterpreterM CExpr
eval env (CRef name) = do
    expr <- envLookup env name
    eval env expr
eval env val@(CVal (CLit _)) = return val
eval env (CVal (CDat name exprs)) = do
    evaled <- mapM (eval env) exprs
    return $ CVal (CDat name evaled)
eval env (CLam p body) = return . Closure $ UClosure p body env
eval env (CApp f arg) = do
    closure <- eval env f
    argv <- eval env arg
    case closure of
      Closure (UClosure p body cenv) ->
        let nenv = Map.insert p argv cenv in
        eval nenv body
      Closure (PClosure name i args) -> do
          let argvs = args ++ [argv]
          if length argvs == i
             then return $ (fromJust $ Map.lookup name primitiveEnv) argvs
             else return . Closure $ PClosure name i argvs
eval env (CFix e) = eval env (CApp e (CFix e))
eval env c@(Closure _) = return c -- need this because eval usage in CRef case
eval env (CMatch test cases) = do
    etest <- eval env test
    let ((_, nenv_), case_, expr) = head -- FIXME: RUNTIME ERR
                             $ filter (\(c,_,_) -> fst c)
                             $ map (\(c,e) -> (testCase etest c, c, e)) cases
    let nenv = Map.fromList $ map (\(a,b) -> (fromJust a, b)) $ filter (isJust . fst) nenv_
    eval nenv expr

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

testCase :: CExpr -> CPattern -> (Bool, [(Maybe Name, CExpr)])
testCase val@(CVal (CLit (StrLit str))) (CPRegex name rtxt) = (isJust $ ICU.find (ICU.regex [] rtxt) str, [(name, val)])
testCase val@(CVal (CLit litl)) (CPLit name litr) = (litl == litr, [(name, val)])
testCase val@(CVal (CDat _ vals)) (CPUnpack name _ ptrns) =
    let results = zipWith testCase vals ptrns in
    (all (== True) $ map fst results , (name, val) : concatMap snd results)
testCase val (CPBind name) = (True, [(Just name, val)])
testCase val (CPElse name) = (True, [(name, val)])
testCase _ _ = (False, [])


-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

runEval :: Env -> CExpr -> TechneResult CExpr
runEval env expr = runIdentity . runExceptT $ eval env expr

