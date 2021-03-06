module Parser
    ( -- Re-exports
    runStateT
    , get
    , put
    -- Types
    , ParserS (..)
    , ParserM (..)
    -- State related
    , initParserS
    -- Lexers
    , lexeme
    , rword
    , colon
    -- Parsers
    , module_
    , decl
    , expr
    -- Helpers
    , parseFile
    , parseModule
    , parseReplWithState
    , parseExprWithState
    ) where

import TechnePrelude
import Err
import Syntax
import Infer

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
    ParserS { stateFnDefs         :: [FnDef]
            , stateFixity         :: [Fixity]
            , stateParamCounter   :: Int
            } deriving (Show,  Eq)

type TParser = Parsec Void Text
type ParserM a  = StateT ParserS TParser a

-- ----------------------------------------------------------------------------
-- State related functions
-- ----------------------------------------------------------------------------

initParserS :: ParserS
initParserS = ParserS { stateFnDefs = []
                      , stateFixity = []
                      , stateParamCounter = 0
                      }

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

-- ----------------------------------------------------------------------------
-- Helper functions for parsing in general
-- ----------------------------------------------------------------------------

testParser p = parseTest (runStateT p initParserS)
tparse p = parse (runStateT p initParserS)

parseModule :: TParser (Module, ParserS)
parseModule = runStateT module_ initParserS

parseFile :: MonadIO f => TParser c -> String -> f (Either TechneErr c)
parseFile p file = first ParserErr <$> (runParser p file <$> readFile file)

parseReplWithState :: ParserS -> Text -> TechneResult (Repl, ParserS)
parseReplWithState state line = first ParserErr $ parse (runStateT repl state) "<stdin>" line

parseExprWithState :: ParserS -> Text -> TechneResult (Expr, ParserS)
parseExprWithState state line =
    case parse (runStateT expr state) "<stdin>" line of
      Right x -> Right x
      Left y -> Left $ ParserErr y

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
newLine    = symbol "\n"
semicolon  = symbol ";"
comma      = symbol ","
colon      = symbol ":"
dot        = symbol "."
bar        = symbol "|"
lparen     = symbol "("
rparen     = symbol ")"
lbrace     = symbol "{"
rbrace     = symbol "}"
langle     = symbol "<"
rangle     = symbol ">"
lbracket   = symbol "["
rbracket   = symbol "]"
tilde      = symbol "~"
equal      = symbol "="

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
           "let", "or", "while", "when", "use", "from", "data", "fn",
           "concept", "impl", "impls"]

rsymbols :: [Text]
rsymbols = ["->", "=", FieldAccessor]

infixChars :: String
infixChars = "-=_?+*/&^%$!@<>:|≤≥÷¬≠"

-- | Parses given reserved word.
-- rword "if"
rword :: Text -> ParserM ()
rword w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

bareColon = try $ colon >> notFollowedBy (oneOf infixChars)
wFn = void (rword "fn") <|> void (symbol "λ")
wArrow = symbol "->" <|> symbol "→"

-- FIXME: needs better definition
identifier :: ParserM Text
identifier = lexeme $ try (tpack <$> some (alphaNumChar <|> char '_') >>= check)
    where check w
            | w `elem` rwords || w `elem` rsymbols = fail $ show w ++ " is a keyword and cannot be an identifier."
            | otherwise = return w

caseIdent :: ParserM Char -> ParserM Text
caseIdent x = lexeme . try $ do
    ups <- x
    rest <- try identifier <|> return ""
    return $ ups `tcons` rest

upcaseIdent :: ParserM Text
upcaseIdent = caseIdent upperChar

lowcaseIdent :: ParserM Text
lowcaseIdent = caseIdent lowerChar

-- FIXME: needs better definition
infixIdent :: ParserM Text
infixIdent = lexeme . try $ do
    c <- some $ oneOf infixChars
    return $ tpack c

callOpIdent :: ParserM Text
callOpIdent = lexeme . try $ do
    c <- some $ oneOf $ '.':infixChars
    check $ tpack c
    where check w
           | w `elem` rsymbols = fail $ show w ++ " is a predefined operator"
           | otherwise = return w

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
    reverse . map (map toOperator) . groupBy (\f1 f2 -> fixityN f1 == fixityN f2) <$>
     sortBy (\ f1 f2 -> fixityN f1 `compare` fixityN f2)
     <$> gets stateFixity

infixTerm :: Text -> ParserM (Expr -> Expr -> Expr)
infixTerm name = try $ BinExpr <$> symbol name

unaryTerm :: Text -> ParserM (Expr -> Expr)
unaryTerm name = try $ UnExpr <$> symbol name

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
    return $ Import path endpoint

decl :: ParserM Decl
decl = FnDecl <$> fnTop
         <|> DataDecl <$> data_
         <|> ConceptDecl <$> concept
         <|> ImplDecl <$> impl

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

typeWithConstraints :: [Constraint] -> ParserM Scheme
typeWithConstraints cnsts = close <$> typeWithConstraints' cnsts

typeWithConstraints' :: [Constraint] -> ParserM Type
typeWithConstraints' cnsts = genericType
                              <|> esotericType
                              <|> liftM2 (mkType cnsts) identifier typeParams
    where typeParams = optional . angles $ typeWithConstraints' cnsts `sepBy` comma
          appliedType cns typname typprms = applyDataType cns typname $ fromMaybe [] typprms
          mkType cnsts tname typprms  =
              case lookupConstraints tname cnsts of
                []                    -> appliedType TyCon tname typprms
                [TypeConstraint name] -> appliedType TyVar name typprms
                xs                    -> appliedType TyVar tname typprms
                -- TODO: after implementing typeclasses, assign constraints here

          genericType = liftM2 (appliedType TyVar) genericIdent typeParams
          esotericType = typeList <|> try typeTuple <|> typeArr
          typeTuple = do
              tuple <- parens $ typeWithConstraints' [] `sepBy1` comma
              return $ applyTuple tuple
          typeList = do
              tvar <- brackets $ typeWithConstraints' cnsts
              return $ pList tvar
          typeArr = do
              typs <- parens $ typeWithConstraints' cnsts `sepBy1` wArrow
              return $ foldl1 (:->>) typs

-- | Parse `A a`, return (a, A)
constraint :: ParserM Constraint
constraint = do
    concept <- identifier
    name <- identifier
    return $ ConceptConstraint name concept

-- | Parse `A a, B b, C c`
-- returns [(a, A), (b, B)]
constraints :: ParserM [Constraint]
constraints  = constraint `sepBy`  comma
constraints1 = constraint `sepBy1` comma

-- | Parse `A a, B b, C c =>`
constraintsWithArrow :: ParserM [Constraint]
constraintsWithArrow = do
    cnsts <- try constraints <|> return []
    unless (null cnsts) $ void (symbol "=>")
    return cnsts

pattern_ :: [Constraint] -> ParserM Pattern
pattern_ cnsts = do
    bindname <- try (Just <$> identifier <* symbol "@") <|> return Nothing
    ElsePattern  bindname <$ rword "else"
      <|> RestPattern bindname <$ symbol "..."
      <|> fmap (LitPattern bindname) lit
      <|> fmap (RegexPattern bindname) regexLit
      <|> fmap (TuplePattern bindname) (tuple $ pattern_ cnsts)
      <|> fmap (ListPattern bindname) (list $ pattern_ cnsts)
      <|> try (liftM2 (UnpackPattern bindname)
                      dataIdent
                      (try (tuple $ pattern_ cnsts) <|> return (Tuple [])))
      <|> case bindname of
            Just _  -> fail "Cannot bind pattern to itself"
            Nothing -> liftM2 BindPattern identifier (optionalType cnsts)

-- | Parse `a: Type` or `a`, or `~a`. Handles type constraints if given any.
param :: [Constraint] -> ParserM Param
param cnsts = do
    pattrn <- pattern_ cnsts
    return $ Param pattrn

-- TODO: check for name conflicts
params :: [Constraint] -> ParserM [Param]
params cnsts = param cnsts `sepBy` comma

optionalType :: [Constraint] -> ParserM (Maybe Scheme)
optionalType cnsts = optional (bareColon >> typeWithConstraints cnsts)

-- Parse a type parameter
typeconstraint :: ParserM Constraint
typeconstraint = TypeConstraint <$> typeparamIdent

freshAnonParam :: ParserM Text
freshAnonParam = do
    s <- get
    put s { stateParamCounter = stateParamCounter s + 1 }
    return $ "anonparam$" ++ tshow (stateParamCounter s)

-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------

ref :: ParserM Ref
ref = fmap Ref identifier
      <|> fmap PlaceHolder (char '$' >> integer)

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
    return $ Fn Nothing prms body []

-- FIXME: While this[1] is valid, this[2] will produce some inconsistencies
-- with typechecker. Look for infer env (FnApplExpr ... in Infer
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
    first <- fnCallTerm
    pairs <- many $ try (liftM2 (,) oper (try fnAppl <|> refExpr))
    foldlM concatFn first pairs
    where concatFn (TupleExpr tuple) (".", fnappl) =
            return $ fnappl { fnApplTuple = tuple <> fnApplTuple fnappl }
          concatFn expr (".", app) = return $ app `prependFnAppl` IndexedTElem expr
          concatFn expr (op, app) = do
            name <- freshAnonParam
            return $ FnApplExpr (mksRef op)
                                (mkTuple [expr,
                                          mkLambda [mksParam name Nothing]
                                                   (app `prependFnAppl` IndexedTElem (mksRef name))])
          oper = do
              x <- callOpIdent
              hasop <- hasOp x
              if hasop || x `elem` rsymbols
                 then fail "a call operator but found an infix operator or a reserved op"
                 else return x

expr :: ParserM Expr
expr = do
    ops <- buildOpTree
    E.makeExprParser term ops

term :: ParserM Expr
term = when_
         <|> match_
         <|> if_
         <|> try fnCall
         <|> try fnAppl
         <|> lambdaExpr
         <|> litExpr
         <|> listExpr
         <|> try (parens expr)
         <|> tupleExpr
         <|> refExpr

fnCallTerm :: ParserM Expr
fnCallTerm = when_
              <|> match_
              <|> if_
              <|> try fnAppl
              <|> lambdaExpr
              <|> litExpr
              <|> listExpr
              <|> try (parens expr)
              <|> tupleExpr
              <|> refExpr

fnApplTerm :: ParserM Expr
fnApplTerm =  refExpr
                <|> parens expr

-- ----------------------------------------------------------------------------
-- Local exprs
-- ----------------------------------------------------------------------------

refExpr :: ParserM Expr
refExpr = try fieldAccessor <|> (RefExpr <$> ref)
    where fieldAccessor = do
            x1 <- some alphaNumChar
            x2 <- string FieldAccessor
            x3 <- some alphaNumChar
            return . RefExpr . Ref $ tpack x1 ++ x2 ++ tpack x3

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
    predicate <- optional $ try (expr <* rword "is")
    pairs <- liftM2 (,) (mkPred predicate) (wArrow >> expr) `sepBy` comma
    rword "end"
    return $ WhenExpr pairs
    where mkPred pred = case pred of
                         Just x  -> fmap (mkEqCheck x) expr
                         Nothing -> expr

match_ :: ParserM Expr
match_ = do
    rword "match"
    predicate <- expr
    rword "with"
    pairs <- liftM2 (,) (pattern_ []) (wArrow >> expr) `sepBy` comma
    rword "end"
    return $ MatchExpr predicate pairs

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
    rword "end"
    return $ WhenExpr $  ((test, true) : pairs) ++ [(mkBool True, els)]

-- ----------------------------------------------------------------------------
-- Top lvl stuff
-- ----------------------------------------------------------------------------

-- TODO: if it's a product type with no name, gave type const.'s name
-- FIXME: find a better syntax for existentialCnsts
data_ :: ParserM Dat
data_ = do
    rword "data"
    name <- identifier
    typecnsts <- typeconstraint `sepBy` comma
    equal
    existentialCnsts <- constraintsWithArrow
    let cnsts = typecnsts ++ existentialCnsts
    base <- try (parens (dataParams cnsts) <* symbol "=>") <|> return []
    datadefs <- sum cnsts
    return $ Dat name typecnsts (datadefs `prependBase` base)
    where sum     cnsts = product cnsts `sepBy1` bar
          product cnsts = liftM2 (,)
              dataIdent
              (try (parens (dataParams cnsts)) <|> return [])
          dataParam cnsts = do
              name <- identifier
              typ  <- colon >> typeWithConstraints' cnsts
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
    tcnst <- typeconstraint
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

-- FIXME: parse call operators
fnTop :: ParserM Fn
fnTop = do
    rword "let" <|> return ()
    fn <- fnPrefix <|> try fnInfix <|> try fnPostfix <|> fn
    liftM2 fn expr where_
    where where_ = (rword "where" >> decl `sepBy` comma) <|> return []
          fnPrefix = do -- TODO: check definition?
            name <- callOpIdent
            prm <- param []
            equal
            return $ Fn (Just name) [prm]
          fnPostfix = do
            prm <- param []
            name <- callOpIdent
            equal
            return $ Fn (Just name) [prm]
          fnInfix = do
            prm1 <- param []
            name <- callOpIdent
            prm2 <- param []
            equal
            return $ Fn (Just name) [prm1, prm2]
          fn = do
            name <- identifier
            params <- params []
            equal
            return $ Fn (Just name) params

fnDefWithConstraints :: [Constraint] -> ParserM FnDef
fnDefWithConstraints constraints = do
    fnname <- try $ identifier <* bareColon
    lclCnsts <- constraintsWithArrow
    let cnsts = constraints ++ lclCnsts
    types <- typeWithConstraints cnsts
    let fndef = FnDef fnname types
    return fndef

-- | Parses top-level function definitions like `f : A a => a -> B`.
fnDef :: ParserM FnDef
fnDef = fnDefWithConstraints []
