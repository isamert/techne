module Desugar
    ( desugarExpr
    , desugarDecl
    , desugarModule
    ) where

import TechnePrelude
import Syntax
import Parser
import Renamer

import Data.Generics.Uniplate.Data
import Data.Data (Data)

desugarModule :: Module -> Module
desugarModule (Module imports decls) = Module imports (map desugarDecl $ desugarPtrnFns decls)

desugarDecl :: Decl -> Decl
desugarDecl (FnDecl fn@Fn { fnBody = body, fnScope = scope }) =
    desugarRecursive $
      (head . desugarPtrnFns) [FnDecl $ fn { fnBody  = desugarExpr body
                                           , fnScope = map desugarDecl scope }]

desugarDecl x = x

desugarExpr :: Expr -> Expr
desugarExpr = desugarPtrnFnExpr . renamePHs . desugarBinPH . desugarExprPH

-- ----------------------------------------------------------------------------
-- Desugar placeholders to lambdas
-- ----------------------------------------------------------------------------

desugarBinPH :: Expr -> Expr
desugarBinPH = binExprPHtoFn

desugarExprPH  :: Expr -> Expr
desugarExprPH = transform exprPHtoFn

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
exprPHtoFn e@MatchExpr{}  = phToFnChildren e
exprPHtoFn e@WhenExpr{}   = phToFnChildren e
exprPHtoFn e@TupleExpr{}  = phToFnChildren e
exprPHtoFn e@FnApplExpr{} = phToFnChildren e
exprPHtoFn e@FixExpr{}    = phToFnChildren e
exprPHtoFn x              = x

binExprPHtoFn :: Expr -> Expr
binExprPHtoFn e@BinExpr{} = phToFnUniverse e
binExprPHtoFn x = descend binExprPHtoFn x

-- | Turn a PlaceHolder into a Param.
phToParam :: Ref -> Param
phToParam (PlaceHolder n) = mksParam ("$" ++ tshow n) Nothing
phToParam _ = error "This shouldn't have happened"

-- ----------------------------------------------------------------------------
-- | Convert all (RefExpr (PlaceHolder _)) to (RefExpr (Ref _))
-- ----------------------------------------------------------------------------

renamePHs = transform renamer
    where renamer (RefExpr (PlaceHolder x)) = RefExpr $ Ref ("$" ++ tshow x)
          renamer x = x

-- ----------------------------------------------------------------------------
-- Desugar functions with patterns to match exprs
-- ----------------------------------------------------------------------------

desugarPtrnFns :: [Decl] -> [Decl]
desugarPtrnFns decls = filter (not . isFnDecl) decls ++ fnDecls
    where fnDecls =  map ((FnDecl . convertMatch) . map declFn)
                      $ groupBy sameFn
                      $ filter isFnDecl decls
          sameFn (FnDecl Fn { fnName = name1 })
                 (FnDecl Fn { fnName = name2 }) = name1 == name2

desugarPtrnFnExpr :: Expr -> Expr
desugarPtrnFnExpr = transform convertLambdaToMatch
    where convertLambdaToMatch (FnExpr fn) = FnExpr $ convertMatch [fn]
          convertLambdaToMatch e           = e

convertMatch :: [Fn] -> Fn
convertMatch fns@(fn:rest) = if check fns
                                then fn
                                else fn { fnParams = params, fnBody = body }
    where body = MatchExpr test cases
          test = mkTupleExpr $ map (\(Param (BindPattern name _)) -> mksRef name) params
          cases = zip (map mkCase fns) (map fnBody fns)
          params = zipWith (\prm -> \case
                                      (Param (BindPattern _ typ)) -> mksParam prm typ
                                      (Param _)                   -> mksParam prm Nothing)
                           paramSupply
                           (fnParams fn)

          paramSupply = map (\n -> "prm$" ++ tshow n) [0..]
          mkCase (Fn _ prms _ _) = mkTuplePattern $ map paramPtrn prms

          mkTuplePattern []  = error "mkTuplePattern []"
          mkTuplePattern [x] = x
          mkTuplePattern xs  = TuplePattern Nothing $ mkTuple xs
          mkTupleExpr []  = error "mkTupleExpr []"
          mkTupleExpr [x] = x
          mkTupleExpr xs  = TupleExpr $  mkTuple xs


          check [Fn _ prms _ _] = all (== True) $ map (isBindPattern . paramPtrn) prms
          check _               = False

-- ----------------------------------------------------------------------------
-- Detect recursive functions and apply fixpoint op
-- ----------------------------------------------------------------------------

desugarRecursive :: Decl -> Decl
desugarRecursive decl@(FnDecl (Fn (Just name) prms body whr)) =
    if isRecursive decl
       then FnDecl $ Fn (Just name) []
                        (FixExpr $ mkLambda (mksParam name Nothing:prms) body)
                        (map desugarRecursive whr)
       else decl

isRecursive :: Decl -> Bool
isRecursive fndecl = fromRight' $ runRenamer False $ do
    [gfndecl] <- renameDecls [fndecl] emptyGenEnv
    return $ isRecursive' gfndecl
    where isRecursive' (FnDecl (Fn (Just name) prms body scope)) =
            name `elem` [name | (RefExpr (Ref name)) <- universe body]
          isRecursive' _ = False

