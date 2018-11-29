module Frontend.Lexer
    (
    -- Functions
    lexeme
    , spaceConsumer
    , symbol
    , newLine
    , semicolon
    , comma
    , colon
    , dot
    , bar
    , lparen
    , rparen
    , lbrace
    , rbrace
    , langle
    , rangle
    , lbracket
    , rbracket
    , equal
    , parens
    , braces
    , angles
    , brackets
    , charLit
    , regexLit
    , stringLit
    , float
    , integer
    , signedFloat
    , signedInteger
    , rwords
    , rword
    , identifier
    , infixIdent
    , genericIdent
    , dataIdent
    , conceptIdent
    , typeparamIdent
    ) where

import TechnePrelude
import Frontend
import Frontend.AST

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
bar       = symbol "|"
lparen    = symbol "("
rparen    = symbol ")"
lbrace    = symbol "{"
rbrace    = symbol "}"
langle    = symbol "<"
rangle    = symbol ">"
lbracket  = symbol "["
rbracket  = symbol "]"
equal     = symbol "="
tilde     = symbol "~"

parens    = between lparen rparen
braces    = between lbrace rbrace
angles    = between langle rangle
brackets  = between lbracket rbracket

-- | A string literal, like "Hey, I'm a string literal"
-- | Respects the escape sequences.
stringLit :: TParser Text
stringLit = lexeme $ tpack <$> (char '"' >> manyTill L.charLiteral (char '"')) -- FIXME: pack?

-- | A char literal, like 'a' or '\n'
-- | Respects the escape sequences.
charLit :: TParser Char
charLit = lexeme $ char '\'' >> L.charLiteral <* char '\''

-- | A regex literal (used as pattern generally), like `$[a-z]^` (acutes
-- | included)
regexLit :: TParser Text
regexLit = lexeme $ tpack <$> (char '`' >> manyTill L.charLiteral (char '`'))

float         = lexeme L.float
integer       = lexeme L.decimal
signedFloat   = L.signed spaceConsumer float
signedInteger = L.signed spaceConsumer integer

-- TODO: needs to be updated
-- TODO: break those apart: wIf = "if", wThen = "then" ...
rwords :: [Text]
rwords = ["if", "then", "else", "elif", "skip", "return", "and", "is",
                 "or", "while", "when", "use", "from", "data"]

-- | Parses given reserved word.
-- | rword "if"
rword :: Text -> TParser ()
rword w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

-- FIXME: get rid of noneOf
identifier :: TParser Text
identifier = (lexeme . try) (ident >>= check)
    where identifierChar = alphaNumChar <?> "an identifer char" -- FIXME: add other stuff
          ident = tpack <$> some identifierChar -- FIXME: pack? Is there something like (some :: TParser Text)?
          check w
            | w `elem` rwords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | "-" `tisPrefixOf` w = fail $ "Identifiers cannot start with \"-\": " ++ show w
            | otherwise = return w

upcaseIdent :: TParser Text
upcaseIdent = lexeme . try $ liftM2 tcons upperChar identifier

lowcaseIdent :: TParser Text
lowcaseIdent = lexeme . try $ liftM2 tcons lowerChar identifier

infixIdent :: TParser Text
infixIdent = lexeme . try $ do
    c1 <- oneOf infixChars
    c2 <- oneOf infixChars
    c3 <- many $ oneOf infixChars
    return $ tpack (c1 : c2 : c3)
    where infixChars = "=-_?+-*/&^%$!@<>:|" :: String

-- | A generic parameter identifier like ~a.
genericIdent :: TParser Text
genericIdent = lexeme . try $ char '~' >> identifier

dataIdent :: TParser Text
dataIdent = upcaseIdent

conceptIdent :: TParser Text
conceptIdent = upcaseIdent

typeparamIdent :: TParser Text
typeparamIdent = lowcaseIdent
