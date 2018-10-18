module Lexer where

import TechnePrelude

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type ErrorBundle = ParseErrorBundle Text Void


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
    where lineComment = L.skipLineComment "#"
          blockComment = L.skipBlockCommentNested "#>" "<#" -- TODO: also allow #< ># ?
                                                            -- #< ># is normal comment, #> <# is doc comment

-- | After every lexeme, whitespace will be consumed automatically.
lexeme = L.lexeme spaceConsumer

-- | A constant symbol.
-- | Check out 'comma', 'semicolon', 'parens' etc.
-- | This is case-sensetive. (L.symbol' is case-insensetive version.)
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- FIXME: Some of them are probably not needed
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

-- | A string literal, like "Hey, I'm a string literal"
-- | Respects the escape sequences.
stringLiteral :: Parser Text
stringLiteral = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- TODO: needs to be updated
-- TODO: break those apart: wIf = "if", wThen = "then" ...
reservedWords :: [Text]
reservedWords = ["if", "then", "else", "elif", "skip", "return", "and", "is",
                 "or", "while", "when"]

-- | Parses given reserved word.
-- | reservedWord "if"
reservedWord :: Text -> Parser ()
reservedWord w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)


identifierChar :: Parser Char
identifierChar = noneOf ("{}[]()<>=|/+\\\"& " :: String) <?> "an identifer char"

identifier :: Parser Text
identifier = (lexeme . try) (ident >>= check)
    where ident = pack <$> some identifierChar -- FIXME: pack? Is there something like (some :: Parser Text)?
          check w
            | w `elem` reservedWords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | otherwise = return w


