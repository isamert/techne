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
data InferE = UnboundVariable Text
type SymbolTable = Map.Map Name Type
data InferS = InferS { counter :: Int }
type InferM = ExceptT InferE (State InferS)

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)
newtype Subst = Subst (Map.Map TVar IType)
newtype TVar = TV String
data IType
    = TVar TVar
    | TCon Text
    | TArr Type Type

data Scheme = Forall [TVar] IType
