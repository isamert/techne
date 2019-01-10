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

type InterpreterM a = TechneM IO a
type PEnv = Map.Map Text ([CExpr] -> IO CExpr)

-- ----------------------------------------------------------------------------
-- Env
-- ----------------------------------------------------------------------------

primitiveEnv :: PEnv
primitiveEnv = Map.fromList $ map snd createEnv

defaultEnv :: Env
defaultEnv = Map.fromList $ map fst createEnv

createEnv :: [((Text, CExpr), (Text, [CExpr] -> IO CExpr))]
createEnv = [envElem "internalSum" 2 pPlus
            ,envElem "internalSub" 2 pMinus
            ,envElem "internalMul" 2 pMul
            ,envElem "internalDiv" 2 pDiv
            ,envElem "internalArrJoin" 2 pArrJoin
            ,envElem "internalArrPrep" 2 pArrPrep
            ,envElem "internalEq" 2 pEq
            ,envElem "internalErr" 1 pErr
            ,envElem "internalNth" 2 pNth]

envElem :: Text -> Int -> b -> ((Text, CExpr), (Text, b))
envElem name n f = ((name, Closure (PClosure name n []))
                   ,(name, f))

pPlus  [CInt x, CInt y] = return $ CInt (x + y)
pMinus [CInt x, CInt y] = return $ CInt (x - y)
pMul   [CInt x, CInt y] = return $ CInt (x * y)
pDiv   [CInt x, CInt y] = return $ CFlt (fromIntegral x / fromIntegral y)
pArrJoin [CCons x CNil, b] = return $ CCons x b
pArrJoin [CCons x y, b] = do { rest <- pArrJoin [y, b]; return $ CCons x (rest); }
pArrJoin [CStr x, CStr y] = return $ CStr (x ++ y)
pArrPrep [x, y@(CCons _ _)] = return $ CCons x y
pArrPrep [x, CNil] = return $ CCons x CNil
pArrPrep [CChr c, CStr s] = return $ CStr (tcons c s)
pEq [CVal x, CVal y] = return $ cBool (x == y)
pNth [CInt n, CVal (CDat _ elems)] = return $ elems !! (fromIntegral n)
pErr [x] = return $ CStr $ tshow x

-- ----------------------------------------------------------------------------
-- Eval
-- ----------------------------------------------------------------------------

envLookup :: Env -> Name -> InterpreterM CExpr
envLookup env name =
    case Map.lookup name env of
      Just x -> return x
      Nothing -> throwError $ InterpreterErr $ RuntimeError name

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
             then do
                    result <- liftIO $ (fromJust $ Map.lookup name primitiveEnv) argvs
                    return result
             else return . Closure $ PClosure name i argvs
eval env c@(Closure _) = return c -- need this because eval usage in CRef case
eval env (CMatch test cases) = do
    etest <- eval env test
    let ((_, nenv_), case_, expr) = head -- FIXME: RUNTIME ERR
                             $ filter (\(c,_,_) -> fst c)
                             $ map (\(c,e) -> (testCase etest c, c, e)) cases
    let nenv = Map.fromList $ map (\(a,b) -> (fromJust a, b)) $ filter (isJust . fst) nenv_
    eval (Map.union nenv env) expr
eval env (CFix name e) =
    let env' = Map.insert name (CApp (Closure $ UClosure name e env') (CFix name e)) env in
        eval env' e

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

testCase :: CExpr -> CPattern -> (Bool, [(Maybe Name, CExpr)])
testCase val@(CVal (CLit (StrLit str))) (CPRegex name rtxt) = (isJust $ ICU.find (ICU.regex [] rtxt) str, [(name, val)])
testCase val@(CVal (CLit litl)) (CPLit name litr) = (litl == litr, [(name, val)])
testCase val@(CVal (CDat datname vals)) (CPUnpack name uname ptrns)
  | datname /= uname = (False, [])
  | length vals == length ptrns =
      let results = zipWith testCase vals ptrns in
      (all (== True) $ map fst results , (name, val) : concatMap snd results)
  | otherwise = (False, [])
testCase val (CPBind name) = (True, [(Just name, val)])
testCase val (CPElse name) = (True, [(name, val)])
testCase _ _ = (False, [])


-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

runEval :: Env -> CExpr -> IO (TechneResult CExpr)
runEval env expr = runExceptT $ eval env expr

