module Frontend.AST where

import TechnePrelude

-- Some type's for clarification
type Name = Text
type Path = Text
type ConceptName = Text
type DataName = Text


data Import = Import [Path] Name -- from Some.Module use Something
    deriving (Eq, Show)

data Module = Module [Import] [Assign] deriving (Eq, Show)

data Type
    = ConcreteType Name
    | PolyType Name (Maybe [ConceptName]) -- Show a => a
    deriving (Eq, Show)

-- TODO: Pattern
-- FIXME: parameters may be Pattern
data Fn
    = Fn [(Name, Type)] Type Expr -- Fn [Params] ReturnType Body
    deriving (Eq, Show)

newtype Concept
    = Concept [(Name, [Type])]
    deriving (Eq, Show)

data Data
    = ProductData [(Name, Type)]
    | SumData [(Name, [(Name, Type)])]
    deriving (Eq, Show)

-- TODO: add rational?
data Lit
    = Chr  Char
    | Str  Text
    | Int  Integer
    | Flt  Double
    | Frac Rational
    deriving (Eq,Show)

-- | Top-level assignments.
data Assign
    = FunAssign Name Fn
    | DataAssign Name Data
    | ConceptAssign Name Concept
    | ImplAssign ConceptName DataName [(Name, Fn)]
    deriving (Eq, Show)

data Expr
    = IfExpr     (Expr, Expr) [(Expr, Expr)] Expr -- if EXPR then EXPR (elif EXPR then EXPR)... else EXPR
    | WhenExpr   (Maybe Expr) [(Expr, Expr)]      -- when [EXPR is] (EXPR -> EXPR)...
    | FnApplExpr Name [(Name,Expr)]                      -- fn(pName=arg1,pName2=arg2...)
    | LitExpr    Lit
    -- TODO: | PartialAppExpr
    deriving (Eq, Show)
