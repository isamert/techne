import TechnePrelude
import Test.HUnit

import Text.Megaparsec
import Control.Monad.State.Lazy

import Frontend.AST
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
  [ testInfer "id" "(fn a -> a)" (Tv"a" ->> Tv"a")
  , testInfer "parameter" "(fn a@3 -> a)" (T"int" ->> T"int")
  , testInfer "list parameter 1" "(fn [a,1] -> a)" (pList (T"int") ->> T"int")
  , testInfer "list parameter 2" "(fn [a,2,b] -> fn c -> [a,b,c])" (pList (T"int") ->> T"int" ->> pList (T"int"))
  , testInfer "list parameter 3" "(fn [a,b,2] -> fn c -> [a,b,c])" (pList (T"int") ->> T"int" ->> pList (T"int"))
  , testInfer "list" "(fn a -> [1,a])" (T"int" ->> pList (T"int"))
  , testInfer "fst" "(fn (a,b) -> a)" (pTuple2 (Tv"a" `TAp` Tv"b") ->> Tv"a")
  , testInfer "snd" "(fn (a,b) -> b)" (pTuple2 (Tv"a" `TAp` Tv"b") ->> Tv"b")
  , testInfer "tuple param" "(fn (a@2,b) -> a)" (pTuple2 (T"int" `TAp` Tv"a") ->> T"int")
  , testInfer "tuple param 2" "(fn (a,b@[1,2]) -> b)" (pTuple2 (Tv"a" `TAp` pList (T"int")) ->> pList (T"int"))
  ]

-- ----------------------------------------------------------------------------
-- Test utils
-- ----------------------------------------------------------------------------

gparse p input = fst . fromRight' $ parse (runStateT p initParserS) "Test" input
