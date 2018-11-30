module Frontend.Infer where

import TechnePrelude
import Frontend.AST

import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Uniplate.Data
import qualified Data.Map.Strict as Map

-- ----------------------------------------------------------------------------
-- Definitions
-- ----------------------------------------------------------------------------
data TypeError = RetardedType -- xd
type SymbolTable = Map.Map Name Type
data IState = IState { counter :: Int, symTable :: SymbolTable }
type InferM = ExceptT TypeError (State IState)

initIState = IState { counter = 0, symTable = Map.empty }

-- infer :: Either TypeError Expr

freshType :: InferM Type
freshType = do
    s <- get
    put s { counter = counter s + 1 }
    return $ flip PolyType [] $ "t" <> tshow (counter s)

inferExpr :: Expr -> InferM Expr
inferExpr expr = undefined

inferModule :: Module -> InferM Expr
inferModule expr = undefined
-- ----------------------------------------------------------------------------
-- Predefined types
-- ----------------------------------------------------------------------------
typeInt = ConcreteType "Int"
typeStr = ConcreteType "Str"


class TypeOf a where
    typeOf :: a -> Type

instance TypeOf Ref where
    typeOf (Ref name typ) = typ
    typeOf (PlaceHolder _) = error "Unexpected place holder."

instance TypeOf Param where
    typeOf (Param _ typ) = typ
    typeOf (DataParam _ typ) = typ


