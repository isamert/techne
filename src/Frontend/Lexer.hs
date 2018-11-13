module Frontend.Lexer
    ( TParser(..)
    , TErrorBundle(..)
    , TState(..)
    , initTState
    , testParser
    , hasFn
    , getFnSignature
    , getFnReturnType
    -- Re-exports
    , runStateT
    , get
    , put
    -- Functions
    , lexeme
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
    , infixId
    ) where

import TechnePrelude
import Frontend.AST

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State.Lazy

newtype TState = TState { stateFnDefs :: [FnDef] } deriving (Show, Eq)
type TParser a = StateT TState (Parsec Void Text) a
type TErrorBundle = ParseErrorBundle Text Void

initTState = TState []
testParser p = parseTest (runStateT p initTState)

-- ----------------------------------------------------------------------------
-- State related functions
-- ----------------------------------------------------------------------------
hasFn :: Name -> TParser Bool
hasFn fnname = do
    state <- get
    return $ fnname `elem` map fnDefName (stateFnDefs state)

getFnSignature :: Name -> TParser (Maybe FnSignature)
getFnSignature fnname = do
    state <- get
    return $ fmap fnDefSignature . headSafe . filter
      (\(FnDef name sig) -> name == fnname) $ stateFnDefs state


getFnReturnType :: Name -> TParser (Maybe Type)
getFnReturnType fnname = (lastSafe =<<) <$> getFnSignature fnname


-- ----------------------------------------------------------------------------
-- Building blocks
-- ----------------------------------------------------------------------------
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

parens    = between lparen rparen
braces    = between lbrace rbrace
angles    = between langle rangle
brackets  = between lbracket rbracket

-- ----------------------------------------------------------------------------
-- More spesific stuff
-- ----------------------------------------------------------------------------
-- | A string literal, like "Hey, I'm a string literal"
-- | Respects the escape sequences.
stringLit :: TParser Text
stringLit = lexeme $ pack <$> (char '"' >> manyTill L.charLiteral (char '"')) -- FIXME: pack?

-- | A char literal, like 'a' or '\n'
-- | Respects the escape sequences.
charLit :: TParser Char
charLit = lexeme $ char '\'' >> L.charLiteral <* char '\''

-- | A regex literal (used as pattern generally), like `$[a-z]^` (acutes
-- | included)
regexLit :: TParser Text
regexLit = lexeme $ pack <$> (char '`' >> manyTill L.charLiteral (char '`'))

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
    where identifierChar = noneOf ("\n.,:;{}[]()=|/+\\\"& ." :: String) <?> "an identifer char"
          ident = pack <$> some identifierChar -- FIXME: pack? Is there something like (some :: TParser Text)?
          check w
            | w `elem` rwords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | "-" `tisPrefixOf` w = fail $ "Identifiers cannot start with \"-\": " ++ show w
            | otherwise = return w

infixId :: TParser Text
infixId = lexeme $ do
    c1 <- oneOf infixChars
    c2 <- oneOf infixChars
    c3 <- many $ oneOf infixChars
    return $ pack (c1 : c2 : c3)
    where infixChars = "=-_?+-*/&^%$!@<>:|" :: String

