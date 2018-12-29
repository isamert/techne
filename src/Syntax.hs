{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Syntax where

import TechnePrelude

import Data.Data hiding (Fixity)
import qualified Data.Map as Map

infixr `TAp`
infixr :-*>
infixr :->>

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
    = WhenExpr   { whenCases :: [(Expr, Expr)] }
    | MatchExpr  { matchTest  :: Expr
                 , matchCases :: [(Pattern, Expr)]
                 }
    | FnApplExpr { fnApplExpr  :: Expr
                 , fnApplTuple :: Tuple Expr
                 }
    | LitExpr    Lit
    | ListExpr   (List Expr)
    | TupleExpr  (Tuple Expr)
    | FnExpr     { fnExprFn :: Fn }
    | RefExpr    Ref
    | UnExpr      { unExprOp      :: Op
                  , unExprOperand :: Expr
                  }
    | BinExpr     { binExprOp     :: Op
                  , binExprLeft   :: Expr
                  , binExprRight  :: Expr
                  }
    | FixExpr     { fixExprExpr   :: Expr }
    deriving (Show, Eq, Data, Typeable)

-- ----------------------------------------------------------------------------
-- Parameters and related stuff
-- ----------------------------------------------------------------------------

data Constraint
    = ConceptConstraint Name ConceptName -- A a => a
    | TypeConstraint Name                -- concept X of a
    deriving (Show, Eq, Ord, Data, Typeable)

data Scheme = Forall [TVar] Type     deriving (Show, Eq, Ord, Data, Typeable)
data Kind   = Star | KArr Kind Kind  deriving (Show, Eq, Ord, Data, Typeable)
data TVar   = TV Text Kind           deriving (Show, Eq, Ord, Data, Typeable)
data TCon   = TC Text Kind           deriving (Show, Eq, Ord, Data, Typeable)

data Type
    = TVar TVar
    | TCon TCon
    | TAp  Type Type
    deriving (Show, Eq, Ord, Data, Typeable)

data DataParam
    = DataParam { dataPrmName :: Name
                , dataPrmType :: Type
                }
    deriving (Show, Eq, Data, Typeable)

newtype Param = Param { paramPtrn :: Pattern }
    deriving (Show, Eq, Data, Typeable)

data Ref
    = Ref Name            -- a
    | PlaceHolder Integer -- "$1"
    deriving (Show, Eq, Ord, Data, Typeable)

data Pattern
    = BindPattern   Name (Maybe Type)                 -- a
    | ElsePattern   { ptrnName     :: Maybe Name }    -- else ->
    | RestPattern   { ptrnName     :: Maybe Name }    -- ...
    | LitPattern    { ptrnName     :: Maybe Name
                    , ptrnLit      :: Lit
                    }
    | RegexPattern  { ptrnName     :: Maybe Name
                    , ptrnRegex    :: Text
                    }
    | TuplePattern  { ptrnName     :: Maybe Name
                    , ptrnTuple    :: Tuple Pattern
                    }
    | ListPattern   { ptrnName     :: Maybe Name
                    , ptrnList     :: List Pattern
                    }
    | UnpackPattern { ptrnName     :: Maybe Name
                    , ptrnDataName :: Name
                    , ptrnPack     :: Tuple Pattern
                    }
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
data Dat = Dat  { datName         :: Name
                , datTypePrms     :: [Constraint]
                , datConstructors :: [(Name , [DataParam])]
                } deriving (Show, Eq, Data, Typeable)

data FnDef
    = FnDef { fnDefName :: Name
            , fnDefSignature :: FnSignature
            } deriving (Show, Eq, Data, Typeable)

-- | Top-level declarations
data Decl
    = FnDecl      { declFn      :: Fn }
    | DataDecl    { declDat     :: Dat }
    | ConceptDecl { declConcept :: Concept }
    | ImplDecl    { declImpl    :: Impl}
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
    = BinOp { opName :: Name }
    | UnOp  { opName :: Name }
    deriving (Show, Eq, Data, Typeable)

data Fn
    = Fn { fnName       :: Maybe Name
         , fnParams     :: [Param]
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

isBindPattern (BindPattern _ _) = True
isBindPattern  _                = False

isFnDecl (FnDecl Fn{}) = True
isFnDecl _             = False

lookupConstraints typ =
    filter (\case
             (ConceptConstraint name concept) -> name == typ
             (TypeConstraint name) -> name == typ)

prependTuple tuple elem = Tuple (elem : tupleElems tuple)
prependFnAppl fnAppl expr = fnAppl { fnApplTuple = fnApplTuple fnAppl `prependTuple` expr }

-- ----------------------------------------------------------------------------
-- mk/mks (s for simple) (These are generally for expressions)
-- ----------------------------------------------------------------------------

mksParam :: Text -> Maybe Type -> Param
mksParam name typ = Param (BindPattern name typ)

mksRef :: Text -> Expr
mksRef name = RefExpr $ Ref name

mkLambda :: [Param] -> Expr -> Expr
mkLambda prms body = FnExpr $
    Fn { fnName = Nothing
       , fnParams = prms
       , fnBody = body
       , fnScope = [] }

mkTuple :: [a] -> Tuple a
mkTuple xs = Tuple $ map IndexedTElem xs

mkEqCheck :: Expr -> Expr -> Expr
mkEqCheck = BinExpr (BinOp "==")

mkBool :: Bool -> Expr
mkBool True  = mksRef "True"
mkBool False = mksRef "False"

-- ----------------------------------------------------------------------------
-- Predefined patterns for easy access
-- ----------------------------------------------------------------------------

pattern FieldAccessor = "::"

pattern EUnary  name expr         = UnExpr    (UnOp    name) expr
pattern EBinary name exprl exprr  = BinExpr   (BinOp   name) exprl exprr
pattern EList   x                 = ListExpr  (List       x)
pattern ETuple  x                 = TupleExpr (Tuple      x)
pattern EChr    x                 = LitExpr   (ChrLit     x)
pattern EStr    x                 = LitExpr   (StrLit     x)
pattern EInt    x                 = LitExpr   (IntLit     x)
pattern EFlt    x                 = LitExpr   (FltLit     x)
pattern EFrac   x                 = LitExpr   (FracLit    x)
pattern ERef name                 = RefExpr   (Ref     name)
pattern EFn name prms body scope  = FnExpr (Fn name prms body scope)

-- A concrete type
pattern TyCon a s = TCon (TC a s)
pattern TyVar a s = TVar (TV a s)

-- Type definitions
pattern TVarA = TVar (TV "a" Star)
pattern TVarB = TVar (TV "b" Star)
pattern TVarC = TVar (TV "c" Star)

pattern TBool  = T "bool"
pattern TInt   = T "int"
pattern TFloat = T "float"
pattern TChar  = T "char"
pattern TFrac  = T "frac"
pattern TArrow = TyCon "->" (Star :-*> Star :-*> Star)
pattern TList  = TyCon "[]" (Star :-*> Star)

-- A concrete type
pattern T a = TCon (TC a Star)

-- A type scheme for a concrete type
pattern S a = Forall [] a

-- A type variable with kind *
pattern Tv a = (TVar (TV a Star))

-- | Kind of a type like: Star :-*> Star :-*> Star
pattern a :-*> b = KArr a b

-- | Function type like: tInt :->> tInt, synonymous for "->"
pattern a :->> b = TAp (TAp TArrow a) b
