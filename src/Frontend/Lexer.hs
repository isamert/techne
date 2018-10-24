module Frontend.Lexer
    ( TParser(..)
    , TErrorBundle(..)
    , TState(..)
    , initTState
    -- Re-exports
    , runStateT
    , get
    , put
    -- Functions
    , lexeme
    , spaceConsumer
    , symbol
    , newLine
    , equals
    , semicolon
    , comma
    , colon
    , dot
    , lparen
    , rparen
    , lbrace
    , rbrace
    , langle
    , rangle
    , lbracket
    , rbracket
    , parens
    , braces
    , angles
    , brackets
    , charLit
    , stringLit
    , float
    , integer
    , signedFloat
    , signedInteger
    , rwords
    , rword
    , identifier
    ) where

import TechnePrelude
import Frontend.AST

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State.Lazy

newtype TState = TState { fnDefs :: [FnDef] } deriving (Show, Eq)
type TParser a = StateT TState (Parsec Void Text) a
type TErrorBundle = ParseErrorBundle Text Void

initTState = TState [[]]

spaceConsumer :: TParser ()
spaceConsumer = L.space space1 lineComment blockComment
    where lineComment = L.skipLineComment "#"
          blockComment = L.skipBlockCommentNested "#>" "<#" -- TODO: also allow #< ># ?
                                                            -- #< ># is normal comment, #> <# is doc comment

-- | After every lexeme, whitespace will be consumed automatically.
lexeme = L.lexeme spaceConsumer

-- | A constant symbol.
-- | Check out 'comma', 'semicolon', 'parens' etc.
-- | This is case-sensetive. (L.symbol' is case-insensetive version.)
symbol :: Text -> TParser Text
symbol = L.symbol spaceConsumer

--
-- Literals
--
newLine   = symbol "\n"
equals    = symbol ";"
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
lparen    = symbol "("
rparen    = symbol ")"
lbrace    = symbol "{"
rbrace    = symbol "}"
langle    = symbol "<"
rangle    = symbol ">"
lbracket  = symbol "["
rbracket  = symbol "]"

parens    = between lparen rparen
braces    = between lbrace rbrace
angles    = between langle rangle
brackets  = between lbracket rbracket

-- | A string literal, like "Hey, I'm a string literal"
-- | Respects the escape sequences.
stringLit :: TParser Text
stringLit = lexeme $ pack <$> (char '"' >> manyTill L.charLiteral (char '"')) -- FIXME: pack?

-- | A char literal, like 'a' or '\n'
-- | Respects the escape sequences.
charLit :: TParser Char
charLit = char '\'' >> L.charLiteral <* char '\''

float         = lexeme L.float
integer       = lexeme L.decimal
signedFloat   = L.signed spaceConsumer float
signedInteger = L.signed spaceConsumer integer

-- TODO: needs to be updated
-- TODO: break those apart: wIf = "if", wThen = "then" ...
rwords :: [Text]
rwords = ["if", "then", "else", "elif", "skip", "return", "and", "is",
                 "or", "while", "when", "use", "from"]

-- | Parses given reserved word.
-- | rword "if"
rword :: Text -> TParser ()
rword w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

identifier :: TParser Text
identifier = (lexeme . try) (ident >>= check)
    where identifierChar = noneOf ("\n.,;{}[]()<>=|/+\\\"& " :: String) <?> "an identifer char"
          ident = pack <$> some identifierChar -- FIXME: pack? Is there something like (some :: TParser Text)?
          check w
            | w `elem` rwords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | "-" `tisPrefixOf` w = fail $ "Identifiers cannot start with \"-\": " ++ show w
            | otherwise = return w
