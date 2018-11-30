{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Frontend.AST where

import TechnePrelude
import Data.Data

-- Some types for clarification
type Name        = Text
type Path        = Text
type ConceptName = Text
type DataName    = Text
type FnSignature = [Type] -- a: Int, b: String

data Expr
    = IfExpr     (Expr, Expr) [(Expr, Expr)] Expr -- if EXPR then EXPR (elif EXPR then EXPR)... else EXPR
    | WhenExpr   (Maybe Expr) [(Expr, Expr)]      -- when [EXPR is] (EXPR -> EXPR)...
    | FnApplExpr { fnApplName :: Name
                 , fnApplTuple :: Tuple Expr
                 }
    | LitExpr    Lit
    | ListExpr   (List Expr)
    | TupleExpr  (Tuple Expr)
    | FnExpr     Fn
    | RefExpr    Ref                             -- a
    | BinExpr    { binExprOp    :: BinOp
                 , binExprLeft  :: Expr
                 , binExprRight :: Expr }
    | Wrapper Expr
    deriving (Eq, Show, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Parameters and related stuff
-- ----------------------------------------------------------------------------
data Constraint
    = ConceptConstraint Name ConceptName -- A a => a
    | TypeConstraint Name                -- concept X of a
    deriving (Eq, Show, Data, Typeable)

data Param
    = Param Pattern Type
    | DataParam Name Type
    deriving (Eq, Data, Typeable)

data Ref
    = Ref Name Type       -- a
    | PlaceHolder Integer -- "$1"
    deriving (Eq, Show, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Module related stuff
-- ----------------------------------------------------------------------------
data Import = Import [Path] [Name] -- from Some.Module use Foo, Bar;
    deriving (Eq, Show, Data, Typeable)

data Module = Module [Import] [Decl]
    deriving (Eq, Data, Typeable)

data Type
    = ConcreteType Name           -- Int
    | PolyType Name [ConceptName] -- `Show a => a` => PolyType a ["Show"]
    | ConstraintType Name         -- concept X of a reqs f : a -> a => `a` is ContraintType
    | UnknownType                 -- Used as placeholder while parsing.
    deriving (Eq, Data, Typeable)

data Pattern
    = BindPattern Name                   -- a
    | LitPattern Lit                     -- 3, "asd" etc.
    | RegexPattern Text                  -- `$[a-d]^`
    | UnpackPattern Name (Tuple Pattern) -- A(3, b) where A is a data
    deriving (Eq, Data, Typeable)

data Impl
    = Impl ConceptName DataName [Fn]
    deriving (Eq, Show, Data, Typeable)

data Concept
    = Concept Constraint [FnDef]
    deriving (Eq, Show, Data, Typeable)

-- Represents a sum type. If data has only one product type then think like
-- it's just a product type.
newtype Dat = Dat [(Name, [Param])]
    deriving (Eq, Data, Typeable)

data FnDef
    = FnDef { fnDefName :: Name
            , fnDefSignature :: FnSignature
            } deriving (Eq,Show, Data, Typeable)

-- | Top-level declarations
data Decl
    = FnDecl Fn
    | DataDecl Dat
    | ConceptDecl Concept
    | ImplDecl Impl
    deriving (Eq, Show, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------
data Lit
    = Chr  Char
    | Str  Text
    | Int  Integer
    | Flt  Double
    | Frac Rational
    deriving (Eq, Typeable, Data)

newtype Tuple a
   = Tuple { tupleElems :: [TupleElem a] }
    deriving (Eq, Functor, Semigroup, Monoid, Data, Typeable)

data TupleElem a
    = IndexedTElem a
    | NamedTElem Name a
    deriving (Eq, Functor, Data, Typeable)

newtype List a
    = List [a]
    deriving (Eq, Functor, Semigroup, Monoid, Data, Typeable)

data BinOp
    = Add
    | Sub
    | Mult
    | Div
    | Op Name
    deriving (Eq, Data, Typeable)

data Fn
    = Fn { fnParams :: [Param]
         , fnReturnType :: Type
         , fnBody :: Expr
         , fnScope :: [Decl] -- where clause
         } deriving (Eq, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Show instances
-- ----------------------------------------------------------------------------
-- instance Show Expr where
    -- show (IfExpr (c,t) _ els) = "if " ++ show c
                                -- ++ " then " ++ show t
                                -- ++ " else " ++ show els
    --show (WhenExpr _ _) = undefined
    --show (LitExpr lit) = show lit
    --show (ListExpr list) = show list
    --show (TupleExpr tuple) = show tuple
    --show (FnExpr fn) = show fn
    --show (RefExpr name) = tunpack name
    --show (BinExpr op left right) = show left ++ " " ++ show op ++ " " ++ show right
    --show (FnApplExpr name tuple) = show name ++ show tuple

instance Show Module where
    show (Module is as) = tie show "\n" is ++ tie show "\n" as

-- FIXME:
--instance Show Decl where
    --show (Decl name expr) = tunpack name ++ " = " ++ show expr

instance Show Pattern where
    show (BindPattern name)          = tunpack name
    show (LitPattern l)              = show l
    show (RegexPattern r)            = show r
    show (UnpackPattern name tuple)  = tunpack name ++ "(" ++ show tuple ++ ")"

instance Show Lit where
    show (Chr c)  = show c
    show (Str s)  = show s
    show (Int i)  = show i
    show (Flt f)  = show f
    show (Frac f) = show f

instance Show a => Show (Tuple a) where
    show (Tuple elems) = "(" ++ tie show ", " elems ++ ")"

instance Show a => Show (TupleElem a) where
    show (IndexedTElem e)    = show e
    show (NamedTElem name e) = tunpack name ++ "=" ++ show e

instance Show a => Show (List a) where
    show (List xs)    = show xs

instance Show BinOp where
    show Add       = "+"
    show Sub       = "-"
    show Mult      = "*"
    show Div       = "/"
    show (Op name) = tunpack name

instance Show Fn where
    show (Fn params rtype body [])    = "fn "
                                       ++ tie show ", " params
                                       ++ " -> "
                                       ++ show body
    show (Fn params rtype body scope) = show (Fn params rtype body [])
                                       ++ "\n    where "
                                       ++ tie show ",\n          " scope

instance Show Dat where
    show (Dat xs) = "data " ++ tie showSum " | " xs
        where showSum (name, typs) =  tunpack name
                                      ++ " (" ++ tie show ", " typs ++ ")"

instance Show Param where
    show (Param (BindPattern p) typ) = tunpack p ++ ": " ++ show typ
    show (Param p typ)               = show p
    show (DataParam name typ)        = tunpack name ++ ": " ++ show typ

instance Show Type where
    show (ConcreteType name)      = tunpack name
    show (PolyType name concepts) = tunpack name ++ "!"
    show (ConstraintType name)    = tunpack name ++ "!!"
    show UnknownType              = "??"

-- ----------------------------------------------------------------------------
-- Other instances
-- ----------------------------------------------------------------------------
instance Semigroup Expr where
    (TupleExpr e1) <> (TupleExpr e2) = TupleExpr (e1 <> e2)
    (ListExpr  e1) <> (ListExpr  e2) = ListExpr  (e1 <> e2)
    e1 <> e2  = error ("Illegal call: " ++ show e1 ++ " <> " ++ show e2)

instance Ord Ref where
    compare (PlaceHolder x) (PlaceHolder y) = compare x y
    compare _ _ = EQ

-- ----------------------------------------------------------------------------
-- Utility functions
-- ----------------------------------------------------------------------------
isBindPattern (BindPattern _ ) = True

lookupConstraints typ =
    filter (\case
             (ConceptConstraint name concept) -> name == typ
             (TypeConstraint name) -> name == typ)

prependTuple tuple elem = Tuple (elem : tupleElems tuple)
prependFnAppl fnAppl expr = fnAppl { fnApplTuple = fnApplTuple fnAppl `prependTuple` expr }

simpleParam name = Param (BindPattern name)
simpleRef name = RefExpr $ Ref name UnknownType
