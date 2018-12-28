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

prettyTuple    xs      = align (encloseSep lparen rparen comma (map pretty xs))
prettyDataType name xs = pretty name <> align (encloseSep langle rangle comma (map pretty xs))

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

pattern DataType1 name t1          = TAp (TyCon name (Star :-*> Star)) t1
pattern DataType2 name t1 t2       = TAp (TAp (TyCon name (Star :-*> Star :-*> Star)) t1) t2
pattern DataType3 name t1 t2 t3    = TAp (TAp (TyCon name (Star :-*> Star :-*> Star :-*> Star)) t1) (TAp t2 t3)
pattern DataType4 name t1 t2 t3 t4 = TAp (TAp (TyCon name (Star :-*> Star :-*> Star :-*> Star :-*> Star)) t1) (TAp t2 (TAp t3 t4))

instance Pretty Type where
    pretty (TVar (TV tv kind)) = prettyTV tv
    pretty (TCon (TC tc kind)) = pretty tc
    pretty (TAp TArrow t)      = pretty t <+> "->"
    pretty (DataType1 "[]" t)              = brackets $ pretty t
    pretty (DataType2 "(,)" t1 t2)         = prettyTuple [t1,t2]
    pretty (DataType3 "(,,)" t1 t2 t3)     = prettyTuple [t1,t2,t3]
    pretty (DataType4 "(,,,)" t1 t2 t3 t4) = prettyTuple [t1,t2,t3,t4]
    pretty (DataType1 name t1)             = prettyDataType name [t1]
    pretty (DataType2 name t1 t2)
      | name /= "->"                       = prettyDataType name [t1,t2]
    pretty (DataType3 name t1 t2 t3)       = prettyDataType name [t1,t2,t3]
    pretty (DataType4 name t1 t2 t3 t4)    = prettyDataType name [t1,t2,t3,t4]
    pretty (TAp t t') = pretty t <+> pretty t'

instance Pretty Scheme where
    pretty (Forall tvs typ) = "forall" <+> sep (map x tvs)  <> "." <+> pretty typ
        where x (TV tv _) = pretty tv

instance Pretty Expr where
    pretty e = undefined
