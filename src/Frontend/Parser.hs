module Frontend.Parser
    ( module_
    , expr
    , assign
    , repl
    ) where

import TechnePrelude
import Frontend.Lexer
import Frontend.AST

import Text.Megaparsec
import Text.Megaparsec.Char

-- FIXME: delet this
repl :: TParser Text
repl = fmap tshow import_ <|> fmap tshow expr <|> fmap tshow fnDef -- <|> fmap tshow assign

module_ :: TParser Module
module_ = do
    imports <- many import_
    assigns <- many assign
    return $ Module imports assigns

import_ :: TParser Import
import_ = do
    symbol "from"
    path <- identifier `sepBy1` char '.'
    symbol "use"
    endpoint <- identifier
    semicolon
    return $ Import path endpoint

-- TODO: parse params and pass them to subparsers
assign :: TParser Assign
assign = undefined

expr :: TParser Expr
expr = literal
    <|> try when_
    <|> if_

literal :: TParser Expr
literal = LitExpr <$>
    (Str <$> stringLit
    <|> Chr <$> charLit
    <|> Flt <$> try signedFloat
    <|> Int <$> signedInteger)

when_ :: TParser Expr
when_ = do
    rword "when"
    predicate <- optional . try $ expr <* rword "is"
    pairs <- flip sepBy1 (symbol ",") $ do
        lhs <- expr
        symbol "->"
        rhs <- expr
        return (lhs, rhs)
    rword "end"
    return $ WhenExpr predicate pairs


-- FIXME: `then` looks ugly, find something else
if_ :: TParser Expr
if_ = do
    rword "if"
    pred <- expr
    rword "then"
    true <- expr
    pairs <- many . try $ do
        rword "elif"
        pred <- expr
        rword "then"
        true <- expr
        return (pred, true)
    rword "else"
    IfExpr (pred, true) pairs <$> expr


params = many param `sepBy` char ','
param = do
    ident <- identifier
    colon
    typ <- identifier
    return (ident, typ)

-- type :: Maybe Params -> TParser Type
type_ = undefined

definition :: TParser (Name, Type)
definition = do
    ident <- identifier
    colon
    typ <- type_
    return (ident, typ)

dat :: TParser Expr
dat = undefined

sumData :: TParser Expr
sumData = undefined

productData :: TParser Expr
productData = undefined

updateFnDefs :: FnDef -> TParser ()
updateFnDefs def = do
    state <- get
    put $ state {fnDefs = def : fnDefs state}


-- | Parses top-level function definitions.
-- | Also it updates the TState for every function definition.
fnDef :: TParser FnDef
fnDef = do
    fnname <- identifier
    colon
    constraints <- try (flip sepBy1 (lexeme $ char ',') $ do {
        cname <- identifier;
        tname <- identifier;
        return (tname, cname) }) <|> return []
    unless (null constraints) $ void (symbol "=>")
    types <- identifier `sepBy1` symbol "->"
    eol <|> semicolon
    let def = map (mkType constraints) types
    updateFnDefs def
    return def
    where mkType constraints typ
            | startsUpper typ = ConcreteType typ
            | otherwise       = PolyType typ $ lookupAll typ constraints


-- | Parses a top level function. (RHS of the assignment)
topLvlFn :: TParser Expr
topLvlFn = undefined


-- | Parses a lambda function.
fn :: TParser Expr
fn = undefined

