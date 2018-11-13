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
import Control.Monad.Combinators.Expr

-- FIXME: delet this
repl :: TParser Text
repl = fmap tshow import_
    <|> fmap tshow expr
    <|> fmap tshow fnDef
    <|> fmap tshow assign
    <|> fmap tshow pattern_

module_ :: TParser Module
module_ = do
    imports <- many import_
    assigns <- many assign
    return $ Module imports assigns

import_ :: TParser Import
import_ = do
    symbol "from"
    path <- identifier `sepBy1` dot
    symbol "use"
    endpoint <- identifier `sepBy` dot
    semicolon
    return $ Import path endpoint

assign :: TParser Assign
assign = do
    rhs <- rhs
    void equal
    Assign (rhsName rhs) <$> topLvlExpr rhs

-- Parse top level expressions like functions, concepts, data definitions etc.
topLvlExpr :: RHS -> TParser Expr
topLvlExpr rhs = data_ rhs
    <|> fnExpr rhs
    -- <|> concept rhs

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------
fnRhs :: Name -> TParser RHS
fnRhs fnname = getFnSignature fnname >>=
    \case
      Just sig -> do
          pattrns <- pattern_`sepBy` comma
          if length pattrns /= (length sig - 1)
             then fail "Parameter count differs from function definition"
             else return $ RHSFn fnname (zipWith Param pattrns sig)
      Nothing -> fail $ "No function has been defined named" ++ show fnname

-- | Parses rhs of an assignment. i.e:
-- | fnName a: String, b: Int OR
-- | A a, B b
-- | x: Y is threated as a type definition whereas X y is treated as type
-- | constraint. rhs must only have either type constraints or type defs.
rhs :: TParser RHS
rhs = do
    name <- identifier
    try (fnRhs name)
      <|> try (RHSData name <$> constraints1)
      <|> try (RHSFn name <$> params1 [])
      <|> return (RHS name)

-- | Add `def` to function definitions of TState
updateFnDefs :: FnDef -> TParser ()
updateFnDefs def = do
    hasFn <- hasFn (fnDefName def)
    when hasFn (fail "More than one definition for same function.")
    state <- get
    void . put $ state { stateFnDefs = def : stateFnDefs state }

-- | Convert given typ name to Type using constraints.
-- | e.g: cs:  [("a", "Show"), ("a", "Numeric")]
--        typ: a   => PolyType typ ["Show", "Numeric"]
--        typ: Int => ConcreteType "Int"
--        typ: c   => ConcreteType "c"
mkType :: [Constraint] -> Maybe Name -> Type
mkType cnsts typ = case typ of
                      Just typ -> case lookupConstraints typ cnsts of
                                  [] -> ConcreteType typ
                                  xs -> PolyType typ $ map
                                    (\(Constraint name concept) -> concept) xs
                      Nothing -> UnknownType

mkTypes :: [Constraint] -> [Maybe Name] -> [Type]
mkTypes cnsts = map (mkType cnsts)

-- | Parse `A a`, return (a, A)
constraint :: TParser Constraint
constraint = do
    concept <- identifier
    name <- identifier
    return $ Constraint name concept

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
params1 cnsts = param cnsts `sepBy1` comma

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
    try (rword "fn") <|> rword "λ"
    prms <- params []
    rword "->"
    body <- expr
    return $ Fn prms UnknownType body []

fn :: RHS -> TParser Fn
fn (RHSFn name prms) = liftM3 (Fn prms)
                              (fromMaybe UnknownType <$> getFnReturnType name)
                              expr
                              where_
fn (RHS name)        = liftM2 (Fn [] UnknownType) expr where_
fn _                 = fail "Malformed RHS."

where_ :: TParser [Assign]
where_ = try $ (rword "where" >> assign `sepBy` comma) <|> return []

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

-- TODO: Instead of defining precedence rules like haskell, make some chars
-- define precedence rules. Like if an operator contains : then precedence
-- is 7 etc.
ops =
    [ [ InfixL (BinExpr Mult <$ symbol "*")
      , InfixL (BinExpr Div  <$ symbol "/") ]
    , [ InfixL (BinExpr Add  <$ symbol "+")
      , InfixL (BinExpr Sub  <$ symbol "- ") ]
    , [InfixL (try $ BinExpr <$> (Op <$> infixId)) ]
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
-- Top lvl exprs
-- ----------------------------------------------------------------------------
-- FIXME: This is fucked up
data_ :: RHS -> TParser Expr
data_ rhs = do
    rword "data"
    localCnsts <- constraintsWithArrow
    when (null localCnsts) $ void (symbol "=>" <|> return "") -- optional `=>`
    (name, cnsts) <- case rhs of
      RHSData name cnsts -> return (name, cnsts ++ localCnsts)
      RHS name           -> return (name, localCnsts)
      _ -> fail "Data definitions may only have type constraints on RHS."
    dat <- sum cnsts
    DataExpr . Data <$> finalize dat name
    where sum     cnsts = product cnsts `sepBy1` bar
          product cnsts = liftM2 (,)
              (Just <$> identifier <|> return Nothing)
              (parens (dataParams cnsts) <|> return [])
          dataParam cnsts = do
              name <- identifier
              typ <- colon >> identifier
              return $ DataParam name (mkType cnsts (Just typ))
          dataParams cnsts = dataParam cnsts `sepBy1` comma
          finalize dat name
              -- If it's a product data and if it has no name, set the name of
              -- the data same as type name from rhs.
              | length dat == 1 && (isNothing . fst . head) dat =
                  if tnull name
                     then fail "Data definition should have a name."
                     else return $ map (\(a,b) -> (name, b)) dat
              | hasNothing $ map fst dat =
                  fail "Some of the data definitions don't have a name."
              | otherwise = return $ map (\(a,b) -> (fromJust a,b)) dat

concept :: RHS -> TParser Expr
concept rhs = undefined

impl :: RHS -> TParser Expr
impl rhs = undefined

fnExpr :: RHS -> TParser Expr
fnExpr rhs = FnExpr <$> fn rhs

-- | Parses top-level function definitions like `f : A a => a -> B` and
-- | updates the state.
fnDef :: TParser FnDef
fnDef = do
    fnname <- identifier
    colon
    cnsts <- constraintsWithArrow
    types <- identifier `sepBy1` symbol "->"
    eol <|> semicolon
    let fndef = FnDef fnname (mkTypes cnsts $ map Just types)
    updateFnDefs fndef
    return fndef

