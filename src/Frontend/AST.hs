{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Frontend.AST where

import TechnePrelude

import Data.Data hiding (Fixity)
import qualified Data.Map as Map

-- Some types for clarification
type Name        = Text
type Path        = Text
type ConceptName = Text
type DataName    = Text
type FnSignature = [Type]

data Repl
    = ReplExpr   Expr
    | ReplImport Import
    | ReplDecl   Decl
    | ReplFnDef  FnDef
    | ReplFixity [Fixity]
    deriving (Show, Eq, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Main stuff
-- ----------------------------------------------------------------------------
data Expr
    = WhenExpr   { whenTest  :: Maybe Expr
                 , whenCases :: [(Expr, Expr)]
                 }
    | MatchExpr  { matchTest  :: Expr
                 , matchCases :: [(Pattern, Expr)]
                 }
    | FnApplExpr { fnApplName  :: Name
                 , fnApplTuple :: Tuple Expr
                 }
    | LitExpr    Lit
    | ListExpr   (List Expr)
    | TupleExpr  (Tuple Expr)
    | FnExpr     Fn
    | RefExpr    Ref
    | UnExpr      { unExprOp      :: Op
                  , unExprOperand :: Expr }
    | BinExpr     { binExprOp     :: Op
                  , binExprLeft   :: Expr
                  , binExprRight  :: Expr }
    deriving (Show, Eq, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Parameters and related stuff
-- ----------------------------------------------------------------------------
data Constraint
    = ConceptConstraint Name ConceptName -- A a => a
    | TypeConstraint Name                -- concept X of a
    deriving (Show, Eq, Data, Typeable)

data Type
    = ConcreteType Name           -- Int
    | PolyType Name [ConceptName] -- `Show a => a` => PolyType a ["Show"]
    | ConstraintType Name         -- concept X of a reqs f : a -> a => `a` is ContraintType
    | UnknownType                 -- Used as placeholder while parsing.
    deriving (Show, Eq, Ord, Data, Typeable)

data Param
    = Param Pattern Type
    | DataParam Name Type
    deriving (Show, Eq, Data, Typeable)

data Ref
    = Ref { refName :: Name, refType :: Type }  -- a
    | PlaceHolder Integer -- "$1"
    deriving (Show, Eq, Ord, Data, Typeable)

data Pattern
    = BindPattern   Name                              -- a
    | ElsePattern   { ptrnName     :: Maybe Name }    -- else ->
    | RestPattern   { ptrnName     :: Maybe Name }    -- ...
    | LitPattern    { ptrnName     :: Maybe Name
                    , ptrnLit      :: Lit }           -- 3, "asd" etc.
    | RegexPattern  { ptrnName     :: Maybe Name
                    , ptrnRegex    :: Text }          -- `$[a-d]^`
    | TuplePattern  { ptrnName     :: Maybe Name
                    , ptrnTuple    :: Tuple Pattern }
    | ListPattern   { ptrnName     :: Maybe Name
                    , ptrnList     :: List Pattern }
    | UnpackPattern { ptrnName     :: Maybe Name
                    , ptrnDataName :: Name
                    , ptrnPack     :: Tuple Pattern } -- A(3, b) where A is a data
    deriving (Show, Eq, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Module related stuff
-- ----------------------------------------------------------------------------
data Import = Import [Path] [Name] -- from Some.Module use Foo, Bar;
    deriving (Show, Eq, Data, Typeable)

data Module = Module [Import] [Decl]
    deriving (Show, Eq, Data, Typeable)

data Impl
    = Impl ConceptName DataName [Fn]
    deriving (Show, Eq, Data, Typeable)

data Concept
    = Concept Constraint [FnDef]
    deriving (Show, Eq, Data, Typeable)

-- Represents a sum type. If data has only one product type then think like
-- it's just a product type.
data Dat = Dat Name [(Name, [Param])] -- Dat DataTypeName [(DataName, [Param])]
    deriving (Show, Eq, Data, Typeable)

data FnDef
    = FnDef { fnDefName :: Name
            , fnDefSignature :: FnSignature
            } deriving (Show, Eq, Data, Typeable)

-- | Top-level declarations
data Decl
    = FnDecl      Fn
    | DataDecl    Dat
    | ConceptDecl Concept
    | ImplDecl    Impl
    deriving (Show, Eq, Data, Typeable)

data Fixity
    = InfixL  { fixityN :: Integer, fixityName :: Name }
    | InfixR  { fixityN :: Integer, fixityName :: Name }
    | Prefix  { fixityN :: Integer, fixityName :: Name }
    | Postfix { fixityN :: Integer, fixityName :: Name }
    deriving (Show, Eq, Ord, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Primitives
-- ----------------------------------------------------------------------------
data Lit
    = ChrLit  Char
    | StrLit  Text
    | IntLit  Integer
    | FltLit  Double
    | FracLit Rational
    | BoolLit Bool
    deriving (Show, Eq, Typeable, Data)

newtype Tuple a
   = Tuple { tupleElems :: [TupleElem a] }
    deriving (Show, Eq, Functor, Semigroup, Monoid, Data, Typeable)

data TupleElem a
    = IndexedTElem a
    | NamedTElem Name a
    deriving (Show, Eq, Functor, Data, Typeable)

newtype List a
    = List [a]
    deriving (Show, Eq, Functor, Semigroup, Monoid, Data, Typeable)

data Op
    = Add
    | Sub
    | Mult
    | Div
    | BinOp { opName :: Name }
    | UnOp  { opName :: Name }
    deriving (Show, Eq, Data, Typeable)

data Fn
    = Fn { fnName       :: Maybe Name
         , fnParams     :: [Param]
         , fnReturnType :: Type
         , fnBody       :: Expr
         , fnScope      :: [Decl] -- where clause
         } deriving (Show, Eq, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Instances
-- ----------------------------------------------------------------------------
instance Semigroup Expr where
    (TupleExpr e1) <> (TupleExpr e2) = TupleExpr (e1 <> e2)
    (ListExpr  e1) <> (ListExpr  e2) = ListExpr  (e1 <> e2)
    e1 <> e2  = error ("Illegal call: " ++ show e1 ++ " <> " ++ show e2)

-- ----------------------------------------------------------------------------
-- utility functions
-- ----------------------------------------------------------------------------
isBindPattern (BindPattern _ ) = True

lookupConstraints typ =
    filter (\case
             (ConceptConstraint name concept) -> name == typ
             (TypeConstraint name) -> name == typ)

prependTuple tuple elem = Tuple (elem : tupleElems tuple)
prependFnAppl fnAppl expr = fnAppl { fnApplTuple = fnApplTuple fnAppl `prependTuple` expr }

-- ----------------------------------------------------------------------------
-- mk/mks (s for simple) (These are generally for expressions)
-- ----------------------------------------------------------------------------
mksParam name = Param (BindPattern name)
mksRef name = RefExpr $ Ref name UnknownType
mkBool x = LitExpr (BoolLit x)

mkLambda :: [Param] -> Expr -> Expr
mkLambda prms body = FnExpr $
    Fn { fnName = Nothing
       , fnParams = prms
       , fnReturnType = UnknownType
       , fnBody = body
       , fnScope = [] }

mkTuple :: [a] -> Tuple a
mkTuple xs = Tuple $ map IndexedTElem xs
