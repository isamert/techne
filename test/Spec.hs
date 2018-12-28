import TechnePrelude
import Test.HUnit

import Text.Megaparsec
import Control.Monad.State.Lazy

import Syntax
import Parser
import Infer

main = runTestTT inferTests


-- ----------------------------------------------------------------------------
-- Inference tests
-- ----------------------------------------------------------------------------

testInfer name exp typ = TestLabel name $ TestCase $ assertEqual name
                                      (inferExpr emptyTypeEnv $ gparse expr exp)
                                      (Right $ generalize emptyTypeEnv typ)

inferTests = TestList
  [ testInfer "id" "(fn a -> a)" (TVarA :->> TVarA)
  , testInfer "parameter" "(fn a@3 -> a)" (TInt :->> TInt)
  , testInfer "list parameter 1" "(fn [a,1] -> a)" (pList TInt :->> TInt)
  , testInfer "list parameter 2" "(fn [a,2,b] -> fn c -> [a,b,c])" (pList TInt :->> TInt :->> pList TInt)
  , testInfer "list parameter 3" "(fn [a,b,2] -> fn c -> [a,b,c])" (pList TInt :->> TInt :->> pList TInt)
  , testInfer "list" "(fn a -> [1,a])" (TInt :->> pList TInt)
  , testInfer "fst" "(fn (a,b) -> a)" (applyTuple [TVarA, TVarB] :->> TVarA)
  , testInfer "snd" "(fn (a,b) -> b)" (applyTuple [TVarA, TVarB] :->> TVarB)
  , testInfer "tuple param" "(fn (a@2,b) -> a)" (applyTuple [TInt, TVarA] :->> TInt)
  , testInfer "tuple param 2" "(fn (a,b@[1,2]) -> b)" (applyTuple [TVarA, pList TInt] :->> pList TInt)
  , testInfer "match and pattern" "(fn a -> match a with 3 -> 3 end)(3)" (TInt)
  , testInfer "match and pattern 2" "(fn a -> match a with b -> [b,3]  end)" (TInt :->> pList TInt)
  , testInfer "match and pattern 3" "(fn a -> match a with b -> [b,3]  end)" (TInt :->> pList TInt)
  ]

-- ----------------------------------------------------------------------------
-- Test utils
-- ----------------------------------------------------------------------------

gparse p input = fst . fromRight' $ parse (runStateT p initParserS) "Test" input
