module Frontend.Desugar (desugar) where

import TechnePrelude
import Frontend.AST
import Frontend.Parser

import Data.Generics.Uniplate.Data
import Data.Data (Data)

-- FIXME: run renamePHs for every individual desugarer, not in here
desugar = renamePHs . desugarBinPH . desugarExprPH

desugarBinPH = binExprPHtoFn
desugarExprPH = transform exprPHtoFn

-- | Convert all (RefExpr (PlaceHolder _)) to (RefExpr (Ref _)).
renamePHs = transform renamer
    where renamer (RefExpr (PlaceHolder x)) = RefExpr $ Ref ("ph$" ++ tshow x) UnknownType
          renamer x = x

-- ----------------------------------------------------------------------------
-- Individual desugarers
-- ----------------------------------------------------------------------------
phToFnChildren = phToFn children
phToFnUniverse = phToFn universe
phToFn f expr = phToFn' $ collectPHs expr
    where phToFn' phs = if not (null phs)
                          then mkLambda (phToParam <$> sort phs) expr
                          else expr
          collectPHs expr = [ph | (RefExpr ph@(PlaceHolder _)) <- f expr]

-- TODO: rename RefExpr immediadetly
exprPHtoFn e@ListExpr{}   = phToFnChildren e
exprPHtoFn e@WhenExpr{}   = phToFnChildren e
exprPHtoFn e@TupleExpr{}  = phToFnChildren e
exprPHtoFn e@FnApplExpr{} = phToFnChildren e
exprPHtoFn x              = x

-- FIXME: a + b($1 == 3) becomes fn $1 -> a + b(...
-- Here are the rules:
-- a + $1                    => fn $1 -> expr
-- a($1)                     => fn $1 -> expr
-- a($1 == 2)                => a(fn $1 -> $1 == 2)
-- $1 + a($2)                => fn $1, $2 -> expr
-- 1 + a($1 == 2)            => 1 + a(fn $1 -> $1 == 2)
-- if $1 then a else b       => fn $1 -> expr
-- if $1 then $1 == b else c => fn $1 -> if $1 then fn $1 -> $1 == b else c
-- [$1, $2]                  => fn $1, $2 -> expr
-- ... add other cases
binExprPHtoFn e@BinExpr{} = phToFnUniverse e
binExprPHtoFn x = descend binExprPHtoFn x

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------
-- | Turn a PlaceHolder into a Param.
phToParam :: Ref -> Param
phToParam (PlaceHolder n) = mksParam ("$" ++ tshow n) UnknownType
phToParam _ = error "This shouldn't have happened"

mkLambda :: [Param] -> Expr -> Expr
mkLambda prms body = FnExpr $
    Fn { fnParams = prms
       , fnReturnType = UnknownType
       , fnBody = body
       , fnScope = [] }
