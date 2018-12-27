module Pretty
    ( pretty
    ) where

import TechnePrelude hiding ((<>))
import Syntax

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

prettyTV tv = pretty $ "~" ++ tv
prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
prettyDecl n tys = pretty n <+> prettyType tys

flattenKind Star         = [Star]
flattenKind (KArr k1 k2) = flattenKind k1 ++ flattenKind k2

-- :kind Map => Map :: * -> * -> *
prettyKind :: Type -> Doc a
prettyKind = undefined

instance Pretty Kind where
    pretty kind = prettyType $ map (const "*") (flattenKind kind)

instance Pretty TVar where
    pretty (TV tv kind) = prettyTV tv <+> pretty kind

instance Pretty TCon where
    pretty (TC tv kind) = pretty tv <+> pretty kind

pattern TArrow = (TCon (TC "->" (KArr Star (KArr Star Star))))
pattern TList = (TCon (TC "[]" (KArr Star Star)))
instance Pretty Type where
    pretty (TVar (TV tv kind)) = prettyTV tv
    pretty (TCon (TC tc kind)) = pretty tc
    pretty (TAp TList t)       = "[" ++ pretty t ++ "]"
    pretty (TAp TArrow t)      = pretty t <+> "->"
    pretty (TAp t t')          = pretty t <+> pretty t'

instance Pretty Scheme where
    pretty (Forall tvs typ) = "forall" <+> sep (map x tvs)  <> "." <+> pretty typ
        where x (TV tv _) = pretty tv

instance Pretty Expr where
    pretty e = undefined
