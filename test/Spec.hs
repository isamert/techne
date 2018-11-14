import TechnePrelude
import Test.HUnit

import Text.Megaparsec
import Control.Monad.State.Lazy

import Frontend.AST
import qualified Frontend.Lexer as L
import qualified Frontend.Parser as P

--
-- Utility
--
runp p = parse (runStateT p L.initTState) "Tests"
pAssertStateless name p input eq = TestCase $ assertEqual name
    (case runp p input of
       Right a -> a
       Left a  -> error "I'm a cute little error.") (eq, L.initTState)

--
-- Lexer tests
--
test1 = pAssertStateless "L.infixId"
                         L.infixId
                         "<>"
                         "<>"

lexerTests = TestList [TestLabel "Lexer tests" test1]


--
-- Parser tests
--


main = runTestTT lexerTests
