module Frontend.Parser
    ( module_
    , expr
    , decl
    , repl
    ) where

import TechnePrelude
import Frontend
import Frontend.Lexer
import Frontend.AST

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Data.Set as Set

-- FIXME: delet this
repl :: TParser Text
repl = fmap tshow import_
    <|> fmap tshow fnDef
    <|> fmap tshow decl
    <|> fmap tshow expr

module_ :: TParser Module
module_ = do
    imports <- many import_
    decls <- many (many (fnDef >>= updateFnDefs) >> decl)
    eof
    return $ Module imports decls

import_ :: TParser Import
import_ = do
    rword "from"
    path <- identifier `sepBy1` dot
    rword "use"
    endpoint <- identifier `sepBy` dot
    semicolon
    return $ Import path endpoint

decl :: TParser Decl
decl = FnDecl <$> fnTop
         <|> DataDecl <$> data_
         <|> ConceptDecl <$> concept
         <|> ImplDecl <$> impl

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------
-- | Convert given typ name to Type using constraints.
-- | e.g: cs:  [("a", "Show"), ("a", "Numeric")]
--        typ: a   => PolyType typ ["Show", "Numeric"]
--        typ: Int => ConcreteType "Int"
--        typ: c   => ConcreteType "c"
mkType :: [Constraint] -> Maybe Name -> Type
mkType cnsts typ = case typ of
    Just typ -> case lookupConstraints typ cnsts of
         [] -> ConcreteType typ
         [TypeConstraint name] -> ConstraintType typ
         xs -> PolyType typ $ map (\case
             ConceptConstraint _ cncpt -> cncpt
             TypeConstraint _ -> error "YOU FUCKED UP") xs
             -- FIXME: needs better error handling
             -- Maybe do this in monad and fail
    Nothing -> UnknownType

mkTypes :: [Constraint] -> [Maybe Name] -> [Type]
mkTypes cnsts = map (mkType cnsts)

-- | Parse `A a`, return (a, A)
constraint :: TParser Constraint
constraint = do
    concept <- identifier
    name <- identifier
    return $ ConceptConstraint name concept

-- | Parse `A a, B b, C c`
-- | returns [(a, A), (b, B)]
constraints :: TParser [Constraint]
constraints  = constraint `sepBy`  comma
constraints1 = constraint `sepBy1` comma

-- | Parse `A a, B b, C c =>`
constraintsWithArrow :: TParser [Constraint]
constraintsWithArrow = do
    cnsts <- try constraints <|> return []
    unless (null cnsts) $ void (symbol "=>")
    return cnsts

pattern_ :: TParser Pattern
pattern_ = LitPattern <$> lit
    <|> RegexPattern <$> regexLit
    <|> try unpackPattern
    <|> BindPattern <$> identifier
    where unpackPattern =
            liftM2 UnpackPattern identifier $ tuple pattern_

-- | Parse `a: Type`
-- | Handles type constraints if given any.
param :: [Constraint] -> TParser Param
param cnsts = do
    pattrn <- pattern_
    typ <- optional (colon >> identifier)
    return $ Param pattrn (mkType cnsts typ)

params :: [Constraint] -> TParser [Param]
params  cnsts = param cnsts `sepBy`  comma

-- Parse a type parameter, or as I recently taken call type constraint
typeconst :: TParser Constraint
typeconst = TypeConstraint <$> typeparamIdent

-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------
tuple :: TParser a -> TParser (Tuple a)
tuple p = Tuple <$> parens (tupElem `sepBy` comma)
    where tupElem = do ident <- optional $ try (identifier <* equal)
                       case ident of
                         Just id -> NamedTElem id <$> p
                         Nothing -> IndexedTElem <$> p

list :: TParser a -> TParser (List a)
list p = List <$> brackets (p `sepBy` comma)

lit :: TParser Lit
lit = Str <$> stringLit
    <|> Chr <$> charLit
    <|> Flt <$> try signedFloat
    <|> Int <$> signedInteger

-- | Parses a lambda function.
lambda :: TParser Fn
lambda = do
    rword "fn" <|> rword "λ"
    prms <- params []
    rword "->"
    body <- expr
    return $ Fn prms UnknownType body []

fnAppl :: TParser Expr
fnAppl = liftM2 FnApplExpr
                identifier
                (tuple expr)

-- FIXME: (1,2).a() is turned into a(1,2). What if a takes (Int, Int) tuple as
-- arg? (btw a((1,2)) works fine)
fnCall :: TParser Expr
fnCall = do
    first <- try (parens expr) <|> fnCallTerm
    dot
    app <- fnAppl `sepBy` dot
    return $ foldl concatFn first app
    where concatFn (TupleExpr tuple) fnappl =
            fnappl { fnApplTuple = tuple <> fnApplTuple fnappl }
          concatFn b a = a `prependFnAppl` IndexedTElem b
          -- These are the terms that does not require parens wrapping
          -- like 1.a() is legal but when ... end.b() is not, it needs parens.
          fnCallTerm = when_
                       <|> if_
                       <|> try fnAppl
                       <|> litExpr
                       <|> listExpr
                       <|> tupleExpr
                       <|> lambdaExpr
                       <|> RefExpr <$> identifier

expr :: TParser Expr
expr = makeExprParser term ops

term :: TParser Expr
term = when_
         <|> if_
         <|> try fnCall
         <|> try fnAppl
         <|> litExpr
         <|> listExpr
         <|> try (parens expr)
         <|> tupleExpr
         <|> lambdaExpr
         <|> RefExpr <$> identifier

-- TODO: Create user defined suffix operators.
--       For example define a operator named `?` inside a concept like
--       Questionable which applies the next function call the value inside `a`
--       So the problem is how the hell ? operator knows about the next
--       function call? What about two types of suffix functions. One works
--       like an infix function and one works like just a suffix.
--       God help me, this is getting out of control.
--       What about ?. being an infix function? Does it work with chaining
--       function calls? Investigate.
-- TODO: Create user defined prefix operators.
-- TODO: Instead of defining precedence rules like haskell, make some chars
-- define precedence rules. Like if an operator contains : then precedence
-- is 7 etc.
ops =
    [ [ InfixL (BinExpr Mult <$ symbol "*")
      , InfixL (BinExpr Div  <$ symbol "/") ]
    , [ InfixL (BinExpr Add  <$ symbol "+")
      , InfixL (BinExpr Sub  <$ symbol "- ") ]
    , [InfixL (try $ BinExpr <$> (Op <$> infixIdent)) ]
    ]

-- ----------------------------------------------------------------------------
-- Local exprs
-- ----------------------------------------------------------------------------
tupleExpr :: TParser Expr
tupleExpr = TupleExpr <$> tuple expr

listExpr :: TParser Expr
listExpr = ListExpr <$> list expr

litExpr :: TParser Expr
litExpr = LitExpr <$> lit

lambdaExpr :: TParser Expr
lambdaExpr = FnExpr <$> lambda

-- FIXME: RHS of the `->` should be a pattern.
-- What about bools? Should I accept them directly or do they need an extra
-- guard or something like that?
when_ :: TParser Expr
when_ = do
    try $ rword "when"
    predicate <- optional . try $ expr <* rword "is"
    pairs <- liftM2 (,) expr (symbol "->" >> expr) `sepBy` comma
    rword "end"
    return $ WhenExpr predicate pairs

-- FIXME: `then` looks ugly, find something else
if_ :: TParser Expr
if_ = do
    try $ rword "if"
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

-- ----------------------------------------------------------------------------
-- Top lvl stuff
-- ----------------------------------------------------------------------------
-- TODO: if it's a product type with no name, gave type const.'s name
-- FIXME: find a better syntax for existentialCnsts
data_ :: TParser Data
data_ = do
    rword "data"
    name <- dataIdent
    typecnsts <- typeconst `sepBy` comma
    equal
    existentialCnsts <- constraintsWithArrow
    let cnsts = typecnsts ++ existentialCnsts
    base <- try (parens (dataParams cnsts) <* symbol "=>") <|> return []
    datadefs <- sum cnsts
    return $ Data (datadefs `prependBase` base)
    where sum     cnsts = product cnsts `sepBy1` bar
          product cnsts = liftM2 (,)
              identifier
              (parens (dataParams cnsts))
          dataParam cnsts = do
              name <- identifier
              typ  <- colon >> identifier
              return $ DataParam name (mkType cnsts (Just typ))
          dataParams cnsts = dataParam cnsts `sepBy1` comma
          prependBase dat base = map (fmap (base ++)) dat

-- TODO: add default impls (maybe using def/default keyword)
-- fnDefs
concept :: TParser Concept
concept = do
    rword "concept"
    name <- conceptIdent
    rword "of"
    tcnst <- typeconst
    reqs <- some ((rword "requires" <|> rword "reqs")
                    >> fnDefWithConstraints [tcnst])
    return $ Concept tcnst reqs

impl :: TParser Impl
impl = do
    rword "impl"
    cname <- dataIdent
    rword "for"
    dname <- dataIdent
    fns <- many (rword "impls" >> fnTop) -- FIXME: better keyword pls
    return $ Impl cname dname fns

fnTop :: TParser Fn
fnTop = do
    name <- identifier
    params <- getFnSignature name >>= \case
        Just sig -> pattern_`sepBy` comma >>= \pattrns ->
            -- FIXME: What if function is in dotfree notation?:
            if length pattrns /= (length sig - 1)
              then fail "Parameter count differs from function definition"
              else return $ zipWith Param pattrns sig
        Nothing -> params []
    equal
    liftM3 (Fn params)
           (fromMaybe UnknownType <$> getFnReturnType name) expr where_
    where where_ = (rword "where" >> decl `sepBy` comma) <|> return []

fnDefWithConstraints :: [Constraint] -> TParser FnDef
fnDefWithConstraints constraints = do
    fnname <- try $ identifier <* colon
    lclCnsts <- constraintsWithArrow
    let cnsts = constraints ++ lclCnsts
    types <- identifier `sepBy1` symbol "->"
    eol <|> semicolon
    let fndef = FnDef fnname (mkTypes cnsts $ map Just types)
    return fndef

-- | Parses top-level function definitions like `f : A a => a -> B`.
fnDef :: TParser FnDef
fnDef = fnDefWithConstraints []
