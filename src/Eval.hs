module Eval where

import TechnePrelude
import Syntax
import Core

import Control.Monad.Identity
import Control.Monad.Except
import qualified Data.Map as Map

-- ----------------------------------------------------------------------------
-- Data definitions
-- ----------------------------------------------------------------------------

data InterpreterE = RuntimeError deriving (Show, Eq)
type InterpreterM a = ExceptT InterpreterE Identity a

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

eval :: Env -> CExpr -> InterpreterM CExpr
eval env (CRef name) = do
    expr <- envLookup env name
    eval env expr
eval env val@(CVal _) = return val
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
eval env c@(Closure _) = return c -- This is here because eval usage in CRef case

-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

runEval :: Env -> CExpr -> Either InterpreterE CExpr
runEval env expr = runIdentity . runExceptT $ eval env expr

envLookup :: Env -> Name -> InterpreterM CExpr
envLookup env name =
    case Map.lookup name env of
      Just x -> return x
      Nothing -> throwError RuntimeError

