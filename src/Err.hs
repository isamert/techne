module Err where

import TechnePrelude
import Syntax

import Control.Monad.Except
import Text.Megaparsec
import Data.Void (Void)

data InferErr
    = UnboundVariable Text
    | UnificationFail Type Type
    | InfiniteType TVar Type
    | KindMismatch TVar Type
    | NotAnExpression Decl
    | OtherError Text
    deriving (Show, Eq)

type ParserErr      = ParseErrorBundle Text Void
newtype RenamerErr  = NotAssigned Text deriving (Show, Eq, Ord)
data CoreErr        = DesugaringError deriving (Show, Eq)
data InterpreterErr = RuntimeError Text deriving (Show, Eq)

data TechneErr
    = ParserErr ParserErr
    | InferenceErr InferErr
    | RenamerErr RenamerErr
    | InterpreterErr InterpreterErr
    | CoreErr CoreErr
    deriving (Show, Eq)

type TechneM m = ExceptT TechneErr m
type TechneResult r = Either TechneErr r
