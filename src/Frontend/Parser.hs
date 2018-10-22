module Frontend.Parser where

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
    signSemicolon
    return $ Import path endpoint

parseAssign :: TParser Assign
parseAssign = undefined


parseExpr :: TParser Expr
parseExpr = parseLitExpr
    <|> parseWhen

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
