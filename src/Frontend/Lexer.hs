module Frontend.Lexer
    ( TParser
    , TParserT
    , TErrorBundle
    , spaceConsumer
    , symbol
    , symNewLine
    , symEquals
    , symSemicolon
    , symComma
    , symColon
    , symDot
    , symLParen
    , symRParen
    , symLBrace
    , symRBrace
    , symLAngle
    , symRAngle
    , symLBracket
    , symRBracket
    , parens
    , braces
    , angles
    , brackets
    , charLiteral
    , stringLiteral
    , float
    , integer
    , signedFloat
    , signedInteger
    , reservedWords
    , reservedWord
    , identifier
    ) where

import TechnePrelude

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type TParser = Parsec Void Text
type TParserT = ParsecT Void Text
type TErrorBundle = ParseErrorBundle Text Void


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
symNewLine   = symbol "\n"
symEquals    = symbol ";"
symSemicolon = symbol ";"
symComma     = symbol ","
symColon     = symbol ":"
symDot       = symbol "."
symLParen    = symbol "("
symRParen    = symbol ")"
symLBrace    = symbol "{"
symRBrace    = symbol "}"
symLAngle    = symbol "<"
symRAngle    = symbol ">"
symLBracket  = symbol "["
symRBracket  = symbol "]"

parens    = between symLParen symRParen
braces    = between symLBrace symRBrace
angles    = between symLAngle symRAngle
brackets  = between symLBracket symRBracket

-- | A string literal, like "Hey, I'm a string literal"
-- | Respects the escape sequences.
stringLiteral :: TParser Text
stringLiteral = pack <$> (char '"' >> manyTill L.charLiteral (char '"')) -- FIXME: pack?

-- | A char literal, like 'a' or '\n'
-- | Respects the escape sequences.
charLiteral :: TParser Char
charLiteral = char '\'' >> L.charLiteral <* char '\''

float         = lexeme L.float
integer       = lexeme L.decimal
signedFloat   = L.signed spaceConsumer float
signedInteger = L.signed spaceConsumer integer

-- TODO: needs to be updated
-- TODO: break those apart: wIf = "if", wThen = "then" ...
reservedWords :: [Text]
reservedWords = ["if", "then", "else", "elif", "skip", "return", "and", "is",
                 "or", "while", "when", "use", "from"]

-- | Parses given reserved word.
-- | reservedWord "if"
reservedWord :: Text -> TParser ()
reservedWord w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

identifier :: TParser Text
identifier = (lexeme . try) (ident >>= check)
    where identifierChar = noneOf ("\n.;{}[]()<>=|/+\\\"& " :: String) <?> "an identifer char"
          ident = pack <$> some identifierChar -- FIXME: pack? Is there something like (some :: TParser Text)?
          check w
            | w `elem` reservedWords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | otherwise = return w
