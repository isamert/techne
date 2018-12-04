module Frontend.Parser
    ( module_
    , expr
    , decl
    ) where

import TechnePrelude
import Frontend
import Frontend.AST

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as Set

-- ----------------------------------------------------------------------------
-- Lexer
-- ----------------------------------------------------------------------------
spaceConsumer :: TParser ()
spaceConsumer = L.space space1 lineComment blockComment
    where lineComment = L.skipLineComment "#"
          blockComment = L.skipBlockCommentNested "#>" "<#" -- TODO: also allow #< ># ?
                                                            -- #< ># is normal comment, #> <# is doc commen
-- | After every lexeme, whitespace will be consumed automatically.
lexeme = L.lexeme spaceConsumer

-- | A constant symbol.
-- Check out 'comma', 'semicolon', 'parens' etc.
-- This is case-sensetive. (L.symbol' is case-insensetive version.)
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
-- Respects the escape sequences.
stringLit :: TParser Text
stringLit = lexeme $ tpack <$> (char '"' >> manyTill L.charLiteral (char '"')) -- FIXME: pack?

-- | A char literal, like 'a' or '\n'
-- Respects the escape sequences.
charLit :: TParser Char
charLit = lexeme $ char '\'' >> L.charLiteral <* char '\''

-- | A regex literal (used as pattern generally), like `$[a-z]^` (acutes
-- included)
regexLit :: TParser Text
regexLit = lexeme $ tpack <$> (char '`' >> manyTill L.charLiteral (char '`'))

float         = lexeme L.float
integer       = lexeme L.decimal
signedFloat   = L.signed spaceConsumer float
signedInteger = L.signed spaceConsumer integer

-- TODO: needs to be updated
rwords :: [Text]
rwords = ["if", "then", "else", "elif", "skip", "return", "and", "is",
                 "or", "while", "when", "use", "from", "data"]

-- | Parses given reserved word.
-- rword "if"
rword :: Text -> TParser ()
rword w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

wFn = void (rword "fn") <|> void (symbol "λ")
wArrow = symbol "->" <|> symbol "→"

-- FIXME: needs better definition
identifier :: TParser Text
identifier = lexeme $ try (tpack <$> some alphaNumChar >>= check)
    where check w
            | w `elem` rwords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | otherwise = return w

upcaseIdent :: TParser Text
upcaseIdent = lexeme . try $ liftM2 tcons upperChar identifier

lowcaseIdent :: TParser Text
lowcaseIdent = lexeme . try $ liftM2 tcons lowerChar identifier

-- FIXME: needs better definition
infixIdent :: TParser Text
infixIdent = lexeme . try $ do
    c1 <- oneOf infixStarters
    c2 <- oneOf infixChars
    c3 <- many $ oneOf infixChars
    return $ tpack (c1 : c2 : c3)
    where infixChars = "-=_?+*/&^%$!@<>:|" :: String
          infixStarters = tail infixChars

-- | A generic parameter identifier like ~a.
genericIdent :: TParser Text
genericIdent = lexeme . try $ char '~' >> identifier

dataIdent :: TParser Text
dataIdent = upcaseIdent

conceptIdent :: TParser Text
conceptIdent = upcaseIdent

typeparamIdent :: TParser Text
typeparamIdent = lowcaseIdent

-- ----------------------------------------------------------------------------
-- Parsers (from here to end of the file)
-- ----------------------------------------------------------------------------
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
typeWithConstraints :: [Constraint] -> TParser Type
typeWithConstraints cnsts =
    (flip PolyType [] <$> genericIdent) <|> do
        tname <- identifier
        case lookupConstraints tname cnsts of
          []                    -> return $ ConcreteType tname
          [TypeConstraint name] -> return $ ConstraintType tname
          xs -> PolyType tname <$> mapM
                 (\case
                   ConceptConstraint _ cncpt -> return cncpt
                   TypeConstraint _ -> fail "A type constraint cannot appear here.") xs

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
pattern_ = do
    bindname <- try (Just <$> identifier <* symbol "@") <|> return Nothing
    ElsePattern  bindname <$ rword "else"
      <|> RestPattern bindname <$ symbol "..."
      <|> fmap (LitPattern bindname) lit
      <|> fmap (RegexPattern bindname) regexLit
      <|> fmap (TuplePattern bindname) (tuple pattern_)
      <|> fmap (ListPattern bindname) (list pattern_)
      <|> try (liftM2 (UnpackPattern bindname) identifier (tuple pattern_))
      <|> case bindname of
            Just _  -> fail "Cannot bind pattern to itself"
            Nothing -> BindPattern <$> identifier

-- | Parse `a: Type` or `a`, or `~a`. Handles type constraints if given any.
param :: [Constraint] -> TParser Param
param cnsts = do
    pattrn <- pattern_
    typ <- optional (colon >> typeWithConstraints cnsts)
    return $ Param pattrn (fromMaybe UnknownType typ)

-- TODO: check for name conflicts
params :: [Constraint] -> TParser [Param]
params cnsts = param cnsts `sepBy` comma

-- Parse a type parameter, or as I recently taken call type constraint
typeconst :: TParser Constraint
typeconst = TypeConstraint <$> typeparamIdent

-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------
-- TODO: forced types?, like a(x : Int, 3)
ref :: TParser Ref
ref = flip Ref UnknownType <$> identifier
      <|> PlaceHolder <$> (char '$' >> integer)

tuple :: TParser a -> TParser (Tuple a)
tuple p = Tuple <$> parens (tupElem `sepBy` comma)
    where tupElem = do ident <- optional $ try (identifier <* equal)
                       case ident of
                         Just id -> NamedTElem id <$> p
                         Nothing -> IndexedTElem <$> p

list :: TParser a -> TParser (List a)
list p = List <$> brackets (p `sepBy` comma)

lit :: TParser Lit
lit = StrLit <$> stringLit
    <|> ChrLit <$> charLit
    <|> FltLit <$> try signedFloat
    <|> IntLit <$> signedInteger

-- | Parses a lambda function.
lambda :: TParser Fn
lambda = do
    wFn
    prms <- params []
    wArrow
    body <- expr
    return $ Fn Nothing prms UnknownType body []

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
                       <|> match_
                       <|> if_
                       <|> try fnAppl
                       <|> litExpr
                       <|> listExpr
                       <|> tupleExpr
                       <|> lambdaExpr
                       <|> refExpr

expr :: TParser Expr
expr = makeExprParser term ops

term :: TParser Expr
term = when_
         <|> match_
         <|> if_
         <|> try fnCall
         <|> try fnAppl
         <|> litExpr
         <|> listExpr
         <|> try (parens expr)
         <|> tupleExpr
         <|> lambdaExpr
         <|> refExpr

-- TODO: Create user defined prefix operators.
-- TODO: Instead of defining precedence rules like haskell, make some chars
-- define precedence rules. Like if an operator contains : then precedence
-- is 7 etc. (crappy idea)
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
refExpr :: TParser Expr
refExpr = RefExpr <$> ref

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
    rword "when"
    predicate <- Just <$> try (expr <* rword "is") <|> return Nothing
    pairs <- liftM2 (,) expr (wArrow >> expr) `sepBy` comma
    rword "end"
    return $ WhenExpr predicate pairs

match_ :: TParser Expr
match_ = do
    rword "match"
    predicate <- expr
    rword "with"
    pairs <- liftM2 (,) pattern_ (wArrow >> expr) `sepBy` comma
    rword "end"
    return $ MatchExpr predicate pairs

-- FIXME: nested if's need an terminator (like end)
if_ :: TParser Expr
if_ = do
    rword "if"
    test <- expr
    true <- rword "then" >> expr
    pairs <- many . try $ do
        pred <- rword "elif" >> expr
        true <- rword "then" >> expr
        return (pred, true)
    els <- rword "else" >> expr
    return $ WhenExpr (Just test) ((mkBool True, true) : pairs)

-- ----------------------------------------------------------------------------
-- Top lvl stuff
-- ----------------------------------------------------------------------------
-- TODO: if it's a product type with no name, gave type const.'s name
-- FIXME: find a better syntax for existentialCnsts
data_ :: TParser Dat
data_ = do
    rword "data"
    name <- dataIdent
    typecnsts <- typeconst `sepBy` comma
    equal
    existentialCnsts <- constraintsWithArrow
    let cnsts = typecnsts ++ existentialCnsts
    base <- try (parens (dataParams cnsts) <* symbol "=>") <|> return []
    datadefs <- sum cnsts
    return $ Dat name (datadefs `prependBase` base)
    where sum     cnsts = product cnsts `sepBy1` bar
          product cnsts = liftM2 (,)
              identifier
              (parens (dataParams cnsts))
          dataParam cnsts = do
              name <- identifier
              typ  <- colon >> typeWithConstraints cnsts
              return $ DataParam name typ
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
    liftM3 (Fn (Just name) params)
           (fromMaybe UnknownType <$> getFnReturnType name)
           expr where_
    where where_ = (rword "where" >> decl `sepBy` comma) <|> return []

fnDefWithConstraints :: [Constraint] -> TParser FnDef
fnDefWithConstraints constraints = do
    fnname <- try $ identifier <* colon
    lclCnsts <- constraintsWithArrow
    let cnsts = constraints ++ lclCnsts
    types <- typeWithConstraints cnsts `sepBy1` wArrow
    eol <|> semicolon
    let fndef = FnDef fnname types
    return fndef

-- | Parses top-level function definitions like `f : A a => a -> B`.
fnDef :: TParser FnDef
fnDef = fnDefWithConstraints []
