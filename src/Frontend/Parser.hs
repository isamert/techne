module Frontend.Parser
    ( parseModule
    , parseExpr
    , parseAssign
    ) where

import TechnePrelude
import Frontend.Lexer
import Frontend.AST

import Text.Megaparsec
import Text.Megaparsec.Char

parseModule :: TParser Module
parseModule = do
    imports <- many parseImport
    assigns <- many parseAssign
    return $ Module imports assigns

parseImport :: TParser Import
parseImport = do
    symbol "from"
    path <- identifier `sepBy1` char '.'
    symbol "use"
    endpoint <- identifier
    symSemicolon
    return $ Import path endpoint

-- TODO: parse params and pass them to subparsers
parseAssign :: TParser Assign
parseAssign = undefined


parseExpr :: TParser Expr
parseExpr = parseLitExpr
    <|> parseWhen
    <|> parseIf

parseLitExpr :: TParser Expr
parseLitExpr = LitExpr <$>
    (Str <$> stringLiteral
    <|> Chr <$> charLiteral
    <|> Flt <$> try signedFloat
    <|> Int <$> signedInteger)

parseWhen :: TParser Expr
parseWhen = do
    reservedWord "when"
    predicate <- optional . try $ parseExpr <* reservedWord "is"
    pairs <- flip sepBy1 (symbol ",") $ do
        lhs <- parseExpr
        symbol "->"
        rhs <- parseExpr
        return (lhs, rhs)
    reservedWord "end"
    return $ WhenExpr predicate pairs

-- FIXME: `then` looks ugly, find something else
parseIf :: TParser Expr
parseIf = do
    reservedWord "if"
    pred <- parseExpr
    reservedWord "then"
    true <- parseExpr
    pairs <- many . try $ do
        reservedWord "elif"
        pred <- parseExpr
        reservedWord "then"
        true <- parseExpr
        return (pred, true)
    reservedWord "else"
    els <- parseExpr
    return $ IfExpr (pred, true) pairs els

-- parseType :: Maybe Params -> TParser Type
parseType = undefined

parseDefinition :: TParser (Name, Type)
parseDefinition = do
    ident <- identifier
    symColon
    typ <- parseType
    return (ident, typ)

parseData :: TParser Expr
parseData = undefined

parseSumData :: TParser Expr
parseSumData = undefined

parseProductData :: TParser Expr
parseProductData = undefined

parseFn :: TParser Expr
parseFn = undefined

parseTopLvlFn :: TParser Expr
parseTopLvlFn = undefined
