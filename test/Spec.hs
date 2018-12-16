import TechnePrelude
import Test.HUnit

import Text.Megaparsec
import Control.Monad.State.Lazy

import Frontend.Syntax
import Frontend.Parser
import Frontend.Infer

main = runTestTT inferTests


-- ----------------------------------------------------------------------------
-- Inference tests
-- ----------------------------------------------------------------------------

testInfer name exp typ = TestLabel name $ TestCase $ assertEqual name
                                      (inferExpr emptyTypeEnv $ gparse expr exp)
                                      (Right $ generalize emptyTypeEnv typ)

inferTests = TestList
  [ testInfer "id" "(fn a -> a)" (tVarA ->> tVarA)
  , testInfer "parameter" "(fn a@3 -> a)" (tInt ->> tInt)
  , testInfer "list parameter 1" "(fn [a,1] -> a)" (pList (tInt) ->> tInt)
  , testInfer "list parameter 2" "(fn [a,2,b] -> fn c -> [a,b,c])" (pList (tInt) ->> tInt ->> pList (tInt))
  , testInfer "list parameter 3" "(fn [a,b,2] -> fn c -> [a,b,c])" (pList (tInt) ->> tInt ->> pList (tInt))
  , testInfer "list" "(fn a -> [1,a])" (tInt ->> pList (tInt))
  , testInfer "fst" "(fn (a,b) -> a)" (applyTuple [tVarA, tVarB] ->> tVarA)
  , testInfer "snd" "(fn (a,b) -> b)" (applyTuple [tVarA, tVarB] ->> tVarB)
  , testInfer "tuple param" "(fn (a@2,b) -> a)" (applyTuple [tInt, tVarA] ->> tInt)
  , testInfer "tuple param 2" "(fn (a,b@[1,2]) -> b)" (applyTuple [tVarA, (pList (tInt))] ->> pList (tInt))
  --, testInfer "match and pattern" "(fn a -> match a with 3 -> 3 end)" (tInt)
  ]

-- ----------------------------------------------------------------------------
-- Test utils
-- ----------------------------------------------------------------------------

gparse p input = fst . fromRight' $ parse (runStateT p initParserS) "Test" input
