{-# LANGUAGE DeriveFunctor, DeriveTraversable,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  FlexibleInstances #-}
module Frontend.AST where

import TechnePrelude

-- Some type's for clarification
type Name = Text
type Path = Text
type ConceptName = Text                         -- a concept name
type FnSignature = [Type]                       -- a: Int, b: String

data Expr
    = IfExpr     (Expr, Expr) [(Expr, Expr)] Expr -- if EXPR then EXPR (elif EXPR then EXPR)... else EXPR
    | WhenExpr   (Maybe Expr) [(Expr, Expr)]      -- when [EXPR is] (EXPR -> EXPR)...
    | FnApplExpr { fnApplName :: Name
                 , fnApplTuple :: Tuple Expr
                 }
    | LitExpr    Lit
    | ListExpr   (List Expr)
    | TupleExpr  (Tuple Expr)
    | DataExpr   Data
    | FnExpr     Fn
    | RefExpr    Name                             -- a
    | BinExpr    { binExprOp    :: BinOp
                 , binExprLeft  :: Expr
                 , binExprRight :: Expr }
    -- TODO: | PartialAppExpr
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Parameters and related stuff
-- ----------------------------------------------------------------------------
data Constraint
    = Constraint { constraintName :: Name
                 , constraintConcept :: Name
                 } deriving (Eq,Show)

data Param
    = Param { paramPattern :: Pattern
            , paramType :: Type
            } deriving (Eq, Show)

-- Data parameters are different from other
-- parameters because other parameters may be
-- patterns while data params cannot.
data DataParam
    = DataParam { dataParamName :: Name
                , dataParamType :: Type
                } deriving (Eq, Show)

-- | Right hand side of an assignment.
data RHS
    = RHSData Name [Constraint]
    | RHSFn   Name [Param]
    | RHS     Name
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Module related stuff
-- ----------------------------------------------------------------------------
data Import = Import [Path] [Name] -- from Some.Module use Foo, Bar;
    deriving (Eq, Show)

data Module = Module [Import] [Assign]
    deriving (Eq, Show)

data Type
    = ConcreteType Name           -- Int
    | PolyType Name [ConceptName] -- `Show a => a` => PolyType a (Just "Show")
    | UnknownType                 -- ...
    deriving (Eq, Show)

data Pattern
    = BindPattern Name                   -- a
    | LitPattern Lit                     -- 3, "asd" etc.
    | RegexPattern Text                  -- `$[a-d]^`
    | UnpackPattern Name (Tuple Pattern) -- A(3, b) where A is a data
    deriving (Eq, Show)


-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------
data Lit
    = Chr  Char
    | Str  Text
    | Int  Integer
    | Flt  Double
    | Frac Rational
    deriving (Eq, Show)

newtype Tuple a
   = Tuple { tupleElems :: [TupleElem a] }
    deriving (Eq, Show, Functor, Semigroup, Monoid)

data TupleElem a
    = IndexedTElem a
    | NamedTElem Name a
    deriving (Eq, Show, Functor)

newtype List a
    = List [a]
    deriving (Eq, Show, Functor, Semigroup, Monoid)

data BinOp
    = Add
    | Sub
    | Mult
    | Div
    | Op Name
    deriving (Eq, Show)

data Fn
    = Fn { fnParams :: [Param]
         , fnReturnType :: Type
         , fnBody :: Expr
         , fnScope :: [Assign] -- where clause
         } deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Top level stuff
-- ----------------------------------------------------------------------------
newtype Concept
    = Concept [(Name, [Type])]
    deriving (Eq, Show)

-- Represents a sum type. If data has only one product type then think like
-- it's just a product type.
newtype Data = Data [(Name, [DataParam])]
    deriving (Eq, Show)

data FnDef
    = FnDef { fnDefName :: Name
            , fnDefSignature :: FnSignature
            } deriving (Eq,Show)

-- | Top-level assignments.
data Assign = Assign Name Expr
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Instances
-- ----------------------------------------------------------------------------
instance Semigroup Expr where
    (TupleExpr e1) <> (TupleExpr e2) = TupleExpr (e1 <> e2)
    (ListExpr  e1) <> (ListExpr  e2) = ListExpr  (e1 <> e2)
    (RefExpr   e1) <> (RefExpr   e2) = RefExpr   (e1 <> e2)
    e1 <> e2  = error ("Illegal call: " ++ show e1 ++ " <> " ++ show e2)

-- ----------------------------------------------------------------------------
-- Utility functions
-- ----------------------------------------------------------------------------
rhsName (RHSData name _) = name
rhsName (RHSFn   name _) = name
rhsName (RHS     name)   = name

isBindPattern (BindPattern _ ) = True

lookupConstraints typ = filter (\(Constraint name concept) -> name == typ)

prependTuple tuple elem = Tuple (elem : tupleElems tuple)
prependFnAppl fnAppl expr = fnAppl { fnApplTuple = fnApplTuple fnAppl `prependTuple` expr }
