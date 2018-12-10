module Frontend.Parser
    ( -- Re-exports
    runStateT
    , get
    , put
    -- Types
    , ParserS (..)
    , ParserM (..)
    , ParserE (..)
    -- State related
    , initParserS
    -- Lexers
    , lexeme
    , rword
    , colon
    -- Parsers
    , module_
    , expr
    -- Helpers
    , parseFile
    , parseModule
    , parseReplWithState
    ) where

import TechnePrelude
import Frontend.AST

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State.Lazy (StateT, runStateT, get, gets, put)
import qualified Control.Monad.Combinators.Expr as E
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as Set

-- ----------------------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------------------
data ParserS =
    ParserS   { stateFnDefs  :: [FnDef]
              , stateFixity  :: [Fixity]
              , stateCounter :: Int
              } deriving (Show, Eq)

type ParserM a  = StateT ParserS (Parsec Void Text) a
type ParserE    = ParseErrorBundle Text Void

-- ----------------------------------------------------------------------------
-- State related functions
-- ----------------------------------------------------------------------------
initParserS :: ParserS
initParserS = ParserS { stateFnDefs = []
                      , stateFixity = []
                      , stateCounter = 0 }

hasFn :: Name -> ParserM Bool
hasFn fnname = do
    state <- get
    return $ fnname `elem` map fnDefName (stateFnDefs state)

-- | Add `def` to function definitions of ParserS
updateFnDefs :: FnDef -> ParserM FnDef
updateFnDefs def = do
    hasFn <- hasFn (fnDefName def)
    when hasFn (fail "More than one definition for same function.")
    state <- get
    put $ state { stateFnDefs = def : stateFnDefs state }
    return def

getFnSignature :: Name -> ParserM (Maybe FnSignature)
getFnSignature fnname = do
    state <- get
    return $ fmap fnDefSignature . headSafe . filter
      (\(FnDef name sig) -> name == fnname) $ stateFnDefs state

getFnReturnType :: Name -> ParserM (Maybe Type)
getFnReturnType fnname = (lastSafe =<<) <$> getFnSignature fnname

-- ----------------------------------------------------------------------------
-- Helper functions for parsing in general
-- ----------------------------------------------------------------------------
testParser p = parseTest (runStateT p initParserS)
tparse p = parse (runStateT p initParserS)
gparse p input = fst . fromRight' $ parse (runStateT p initParserS) "Test" input

parseModule = runStateT module_ initParserS
parseFile p file = runParser p file <$> readFile file
parseReplWithState state = parse (runStateT repl state) "repl-line"

repl :: ParserM Repl
repl = (ReplFixity <$> fixity)
         <|> (ReplImport <$> import_)
         <|> (ReplFnDef <$> (fnDef >>= updateFnDefs))
         <|> (ReplExpr <$> expr)
         <|> (ReplDecl <$> decl)

-- ----------------------------------------------------------------------------
-- Lexer
-- ----------------------------------------------------------------------------
spaceConsumer :: ParserM ()
spaceConsumer = L.space space1 lineComment blockComment
    where lineComment = L.skipLineComment "#"
          blockComment = L.skipBlockCommentNested "#>" "<#" -- TODO: also allow #< ># ?
                                                            -- #< ># is normal comment, #> <# is doc commen
-- | After every lexeme, whitespace will be consumed automatically.
lexeme = L.lexeme spaceConsumer

-- | A constant symbol.
-- Check out 'comma', 'semicolon', 'parens' etc.
-- This is case-sensetive. (L.symbol' is case-insensetive version.)
symbol :: Text -> ParserM Text
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
stringLit :: ParserM Text
stringLit = lexeme $ tpack <$> (char '"' >> manyTill L.charLiteral (char '"')) -- FIXME: pack?

-- | A char literal, like 'a' or '\n'
-- Respects the escape sequences.
charLit :: ParserM Char
charLit = lexeme $ char '\'' >> L.charLiteral <* char '\''

-- | A regex literal (used as pattern generally), like `$[a-z]^` (acutes
-- included)
regexLit :: ParserM Text
regexLit = lexeme $ tpack <$> (char '`' >> manyTill L.charLiteral (char '`'))

float         = lexeme L.float
integer       = lexeme L.decimal
signedFloat   = L.signed spaceConsumer float
signedInteger = L.signed spaceConsumer integer

-- TODO: needs to be updated
rwords :: [Text]
rwords = ["if", "then", "else", "elif", "skip", "return", "and", "is",
           "let", "or", "while", "when", "use", "from", "data"]

-- | Parses given reserved word.
-- rword "if"
rword :: Text -> ParserM ()
rword w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

wFn = void (rword "fn") <|> void (symbol "λ")
wArrow = symbol "->" <|> symbol "→"

-- FIXME: needs better definition
identifier :: ParserM Text
identifier = lexeme $ try (tpack <$> some alphaNumChar >>= check)
    where check w
            | w `elem` rwords = fail $ show w ++ " is a keyword and cannot be an identifier."
            | otherwise = return w

upcaseIdent :: ParserM Text
upcaseIdent = lexeme . try $ liftM2 tcons upperChar identifier

lowcaseIdent :: ParserM Text
lowcaseIdent = lexeme . try $ liftM2 tcons lowerChar identifier

-- FIXME: needs better definition
infixIdent :: ParserM Text
infixIdent = lexeme . try $ do
    c <- many $ oneOf infixChars
    return $ tpack c
    where infixChars = "-=_?+*/&^%$!@<>:|" :: String

-- | A generic parameter identifier like ~a.
genericIdent :: ParserM Text
genericIdent = lexeme . try $ char '~' >> identifier

dataIdent :: ParserM Text
dataIdent = upcaseIdent

conceptIdent :: ParserM Text
conceptIdent = upcaseIdent

typeparamIdent :: ParserM Text
typeparamIdent = lowcaseIdent

-- ----------------------------------------------------------------------------
-- Fixity related functions
-- ----------------------------------------------------------------------------
fixity :: ParserM [Fixity]
fixity = do
    constr <- (rword "infixl" >> return InfixL)
              <|> (rword "infixr" >> return InfixR)
              <|> (rword "postfix" >> return Postfix)
              <|> (rword "prefix" >> return Prefix)
    i <- integer
    ops <- infixIdent `sepBy` comma
    s <- get
    let fs = constr i <$> ops
    put s { stateFixity = fs ++ stateFixity s }
    return fs

buildOpTree :: ParserM [[E.Operator (StateT ParserS (Parsec Void Text)) Expr]]
buildOpTree =
    reverse . map (map toOperator) . groupBy (\ f1 f2 -> fixityN f1 == fixityN f2) <$>
     sortBy (\ f1 f2 -> fixityN f1 `compare` fixityN f2)
     <$> gets stateFixity

infixTerm :: Text -> ParserM (Expr -> Expr -> Expr)
infixTerm name = try $ EBinary <$> symbol name

unaryTerm :: Text -> ParserM (Expr -> Expr)
unaryTerm name = try $ EUnary <$> symbol name

toOperator (InfixL  _ name) = E.InfixL  $ infixTerm name
toOperator (InfixR  _ name) = E.InfixR  $ infixTerm name
toOperator (Prefix  _ name) = E.Prefix  $ unaryTerm name
toOperator (Postfix _ name) = E.Postfix $ unaryTerm name

hasOp :: Name -> ParserM Bool
hasOp name = do
    fs <- gets stateFixity
    return $ any (\f -> name == fixityName f) fs

-- ----------------------------------------------------------------------------
-- Parsers (from here to end of the file)
-- ----------------------------------------------------------------------------
module_ :: ParserM Module
module_ = do
    imports <- many import_
    fs <- many fixity
    decls <- many (many (fnDef >>= updateFnDefs) >> decl)
    eof
    return $ Module imports decls

import_ :: ParserM Import
import_ = do
    rword "from"
    path <- identifier `sepBy1` dot
    rword "use"
    endpoint <- identifier `sepBy` dot
    semicolon
    return $ Import path endpoint

decl :: ParserM Decl
decl = FnDecl <$> fnTop
         <|> DataDecl <$> data_
         <|> ConceptDecl <$> concept
         <|> ImplDecl <$> impl

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------
typeWithConstraints :: [Constraint] -> ParserM Type
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
constraint :: ParserM Constraint
constraint = do
    concept <- identifier
    name <- identifier
    return $ ConceptConstraint name concept

-- | Parse `A a, B b, C c`
-- | returns [(a, A), (b, B)]
constraints :: ParserM [Constraint]
constraints  = constraint `sepBy`  comma
constraints1 = constraint `sepBy1` comma

-- | Parse `A a, B b, C c =>`
constraintsWithArrow :: ParserM [Constraint]
constraintsWithArrow = do
    cnsts <- try constraints <|> return []
    unless (null cnsts) $ void (symbol "=>")
    return cnsts

pattern_ :: ParserM Pattern
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
param :: [Constraint] -> ParserM Param
param cnsts = do
    pattrn <- pattern_
    typ <- optional (colon >> typeWithConstraints cnsts)
    return $ Param pattrn (fromMaybe UnknownType typ)

-- TODO: check for name conflicts
params :: [Constraint] -> ParserM [Param]
params cnsts = param cnsts `sepBy` comma

-- Parse a type parameter, or as I recently taken call type constraint
typeconst :: ParserM Constraint
typeconst = TypeConstraint <$> typeparamIdent

freshName :: ParserM Text
freshName = do
    s <- get
    put s { stateCounter = stateCounter s + 1 }
    return $ "anonparam$" ++ tshow (stateCounter s)

-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------
-- TODO: forced types?, like a(x : Int, 3)
ref :: ParserM Ref
ref = flip Ref UnknownType <$> identifier
      <|> PlaceHolder <$> (char '$' >> integer)

tuple :: ParserM a -> ParserM (Tuple a)
tuple p = Tuple <$> parens (tupElem `sepBy` comma)
    where tupElem = do ident <- optional $ try (identifier <* equal)
                       case ident of
                         Just id -> NamedTElem id <$> p
                         Nothing -> IndexedTElem <$> p

list :: ParserM a -> ParserM (List a)
list p = List <$> brackets (p `sepBy` comma)

lit :: ParserM Lit
lit = StrLit <$> stringLit
    <|> ChrLit <$> charLit
    <|> FltLit <$> try signedFloat
    <|> IntLit <$> signedInteger

-- | Parses a lambda function.
lambda :: ParserM Fn
lambda = do
    wFn
    prms <- params []
    wArrow
    body <- expr
    return $ Fn Nothing prms UnknownType body []

-- FIXME: While this[1] is valid, this[2] will produce some inconsistencies
-- with typechecker. Look for infer env (FnApplExpr ... in Frontend.Infer
-- especially fixOrder function.
-- [1]: (fn a, b, c -> a + b + c)(1)(2)(3)
-- [2]: (fn a, b, c -> a + b + c)(c=3)(1,2) OR other variations with named args
fnAppl :: ParserM Expr
fnAppl = liftM2 FnApplExpr
                fnApplTerm
                args
    where args = do
            xs <- some $ tuple expr
            return $ foldl1 (<>) xs

fnCall :: ParserM Expr
fnCall = do
    first <- try (parens expr) <|> fnCallTerm
    pairs <- many $ try (liftM2 (,) (dot <|> oper) fnAppl)
    foldlM concatFn first pairs
    where concatFn (TupleExpr tuple) (".", fnappl) =
            return $ fnappl { fnApplTuple = tuple <> fnApplTuple fnappl }
          concatFn expr (".", app) = return $ app `prependFnAppl` IndexedTElem expr
          concatFn expr (op, app) = do
            name <- freshName
            return $ FnApplExpr (mksRef op) (mkTuple [expr, mkLambda [mksParam name UnknownType] (app `prependFnAppl` IndexedTElem (mksRef name))])
          oper = do
              x <- infixIdent
              hasop <- hasOp x
              if hasop
                 then fail "a call operator but found an infix operator"
                 else return x

expr :: ParserM Expr
expr = do
    ops <- buildOpTree
    E.makeExprParser term ops

term :: ParserM Expr
term = when_
         <|> match_
         <|> if_
         <|> try fnAppl
         <|> try fnCall
         <|> litExpr
         <|> listExpr
         <|> try (parens expr)
         <|> tupleExpr
         <|> lambdaExpr
         <|> refExpr

fnCallTerm :: ParserM Expr
fnCallTerm = when_
              <|> match_
              <|> if_
              <|> try fnAppl
              <|> litExpr
              <|> listExpr
              <|> tupleExpr
              <|> lambdaExpr
              <|> refExpr

-- FIXME: match_ if_ when_ fnCall may also return a lambda
fnApplTerm :: ParserM Expr
fnApplTerm =  refExpr <|> parens lambdaExpr

-- ----------------------------------------------------------------------------
-- Local exprs
-- ----------------------------------------------------------------------------
refExpr :: ParserM Expr
refExpr = RefExpr <$> ref

tupleExpr :: ParserM Expr
tupleExpr = TupleExpr <$> tuple expr

listExpr :: ParserM Expr
listExpr = ListExpr <$> list expr

litExpr :: ParserM Expr
litExpr = LitExpr <$> lit

lambdaExpr :: ParserM Expr
lambdaExpr = FnExpr <$> lambda

when_ :: ParserM Expr
when_ = do
    rword "when"
    predicate <- Just <$> try (expr <* rword "is") <|> return Nothing
    pairs <- liftM2 (,) expr (wArrow >> expr) `sepBy` comma
    rword "end"
    return $ WhenExpr predicate pairs

match_ :: ParserM Expr
match_ = do
    rword "match"
    predicate <- expr
    rword "with"
    pairs <- liftM2 (,) pattern_ (wArrow >> expr) `sepBy` comma
    rword "end"
    return $ MatchExpr predicate pairs

-- FIXME: nested if's need an terminator (like end)
-- FIXME: else
if_ :: ParserM Expr
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
data_ :: ParserM Dat
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
concept :: ParserM Concept
concept = do
    rword "concept"
    name <- conceptIdent
    rword "of"
    tcnst <- typeconst
    reqs <- some ((rword "requires" <|> rword "reqs")
                    >> fnDefWithConstraints [tcnst])
    return $ Concept tcnst reqs

impl :: ParserM Impl
impl = do
    rword "impl"
    cname <- dataIdent
    rword "for"
    dname <- dataIdent
    fns <- many (rword "impls" >> fnTop) -- FIXME: better keyword pls
    return $ Impl cname dname fns

fnTop :: ParserM Fn
fnTop = do
    rword "let" <|> return ()
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

fnDefWithConstraints :: [Constraint] -> ParserM FnDef
fnDefWithConstraints constraints = do
    fnname <- try $ identifier <* colon
    lclCnsts <- constraintsWithArrow
    let cnsts = constraints ++ lclCnsts
    types <- typeWithConstraints cnsts `sepBy1` wArrow
    eol <|> semicolon
    let fndef = FnDef fnname types
    return fndef

-- | Parses top-level function definitions like `f : A a => a -> B`.
fnDef :: ParserM FnDef
fnDef = fnDefWithConstraints []
