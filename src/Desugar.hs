module Desugar (desugarExpr) where

import TechnePrelude
import Syntax
import Parser

import Data.Generics.Uniplate.Data
import Data.Data (Data)


-- FIXME: run renamePHs for every individual desugarer, not in here
desugarExpr = renamePHs . desugarBinPH . desugarExprPH

desugarBinPH = binExprPHtoFn
desugarExprPH = transform exprPHtoFn

-- | Convert all (RefExpr (PlaceHolder _)) to (RefExpr (Ref _)).
renamePHs = transform renamer
    where renamer (RefExpr (PlaceHolder x)) = RefExpr $ Ref ("$" ++ tshow x)
          renamer x = x

-- ----------------------------------------------------------------------------
-- Individual desugarers
-- ----------------------------------------------------------------------------

phToFn :: (Expr -> [Expr]) -> Expr -> Expr
phToFn f expr = phToFn' $ collectPHs expr
    where phToFn' phs = if not (null phs)
                          then mkLambda (phToParam <$> sort phs) expr
                          else expr
          collectPHs expr = [ph | (RefExpr ph@(PlaceHolder _)) <- f expr]

phToFnChildren :: Expr -> Expr
phToFnChildren = phToFn children

phToFnUniverse :: Expr -> Expr
phToFnUniverse = phToFn universe

exprPHtoFn :: Expr -> Expr
exprPHtoFn e@ListExpr{}   = phToFnChildren e
exprPHtoFn e@WhenExpr{}   = phToFnChildren e
exprPHtoFn e@TupleExpr{}  = phToFnChildren e
exprPHtoFn e@FnApplExpr{} = phToFnChildren e
exprPHtoFn x              = x

binExprPHtoFn :: Expr -> Expr
binExprPHtoFn e@BinExpr{} = phToFnUniverse e
binExprPHtoFn x = descend binExprPHtoFn x

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

-- | Turn a PlaceHolder into a Param.
phToParam :: Ref -> Param
phToParam (PlaceHolder n) = mksParam ("$" ++ tshow n) Nothing
phToParam _ = error "This shouldn't have happened"
