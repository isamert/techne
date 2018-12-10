module Frontend.Infer
    ( inferExpr
    , emptyTypeEnv
    , TypeEnv(..)
    , Subst(..)
    , IType(..)
    , Scheme(..)
    , TVar(..)
    ) where

import TechnePrelude
import Frontend.AST
import Frontend.Parser

import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Uniplate.Data
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--  Adaptation of: http://dev.stephendiehl.com/fun/006_hindley_milner.html

-- ----------------------------------------------------------------------------
-- Definitions
-- ----------------------------------------------------------------------------
newtype TVar = TV Text             deriving (Show, Eq, Ord)
data Scheme  = Forall [TVar] IType deriving (Show, Eq, Ord)

-- | An (i)nferred type
data IType
    = TVar TVar
    | TCon Text
    | TArr IType IType
    deriving (Show, Eq, Ord)

pattern T a          = TCon a
pattern Tv a         = TVar (TV a)
pattern t1    :-> t2 = TArr t1 t2
pattern tvars :=> t  = Forall tvars t

infixr :->
infixr :=>

data InferE
    = UnboundVariable Text
    | UnificationFail IType IType
    | InfiniteType TVar IType
    | ParamCountMismatch [IType] [IType]
    deriving (Show, Eq, Ord)

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)
newtype InferS = InferS { counter :: Int }
type InferM = ExceptT InferE (State InferS)
type Subst = Map.Map TVar IType

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------
initInferS :: InferS
initInferS = InferS 0

extendTypeEnv :: TypeEnv -> (Name, Scheme) -> TypeEnv
extendTypeEnv (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

extendTypeEnvAll :: TypeEnv -> [(Name, Scheme)] -> TypeEnv
extendTypeEnvAll (TypeEnv env) tsps = TypeEnv $ Map.fromList tsps `Map.union` env

fresh :: InferM IType
fresh = do
  s <- get
  put s{counter = counter s + 1}
  return $ TVar $ TV $ "t" ++ tshow (counter s)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

emptySubst :: Subst
emptySubst = Map.empty

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Only used while normalizing.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- ----------------------------------------------------------------------------
-- typeOf
-- ----------------------------------------------------------------------------
class Typed a where
    typeOf :: a -> InferM IType

instance Typed Op where
    typeOf (BinOp  "+") = return $ T"int"   :-> T"int"   :-> T"int"
    typeOf (BinOp  "-") = return $ T"int"   :-> T"int"   :-> T"int"
    typeOf (BinOp  "*") = return $ T"int"   :-> T"int"   :-> T"int"
    typeOf (BinOp  "/") = return $ T"float" :-> T"float" :-> T"float"
    typeOf (BinOp "==") = instantiate $ [TV"a"] :=> Tv"a" :-> Tv"a" :-> T"bool"

instance Typed Lit where
    typeOf (ChrLit  _) = return $ T"char"
    typeOf (StrLit  _) = return $ T"string"
    typeOf (IntLit  _) = return $ T"int"
    typeOf (FltLit  _) = return $ T"float"
    typeOf (FracLit _) = return $ T"frac"
    typeOf (BoolLit _) = return $ T"bool"

-- ----------------------------------------------------------------------------
-- Substitutable
-- ----------------------------------------------------------------------------
class Substitutable a where
    apply :: Subst -> a -> a    -- apply the Subst over expressions
    ftv   :: a -> Set.Set TVar  -- query free variables and return them

instance Substitutable IType where
    apply _ (TCon a)       = TCon a
    apply s t@(TVar a)     = Map.findWithDefault t a s
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2

    ftv (TVar var) = Set.singleton var
    ftv (TCon _  ) = Set.empty
    ftv (t1 :-> t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

-- ----------------------------------------------------------------------------
-- Main functions
-- ----------------------------------------------------------------------------
unify ::  IType -> IType -> InferM Subst
unify (l :-> r) (l' :-> r')  = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `composeSubst` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return emptySubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> IType -> InferM Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Map.singleton a t

instantiate ::  Scheme -> InferM IType
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> IType -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

closeOver :: (Map.Map TVar IType, IType) -> Scheme
closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTypeEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where ord = zip (nub $ fv body) (fmap (TV . tpack) letters)

        fv (TVar a)   = [a]
        fv (a :-> b)  = fv a ++ fv b
        fv (TCon _)   = []

        normtype (a :-> b)  = normtype a :-> normtype b
        normtype (TCon a)   = TCon a
        normtype (TVar a)   =
          case lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

lookupEnv :: TypeEnv -> Name -> InferM (Subst, IType)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (tshow x)
    Just s  -> do t <- instantiate s
                  return (emptySubst, t)

inferPrim :: TypeEnv -> [Expr] -> IType -> InferM (Subst, IType)
inferPrim env l t = do
    tv <- fresh
    (s1, tf) <- foldM inferStep (emptySubst, id) l
    s2 <- unify (apply s1 (tf tv)) t
    return (s2 `composeSubst` s1, apply s2 tv)
    where inferStep (s, tf) exp = do
            (s', t) <- infer (apply s env) exp
            return (s' `composeSubst` s, tf . (t :->))

infer :: TypeEnv -> Expr -> InferM (Subst, IType)
infer env (ERef name _) =  lookupEnv env name

infer env (LitExpr lit)   = do
    typ <- typeOf lit
    return (emptySubst, typ)

infer env (BinExpr op e1 e2)   = do
    typ <- typeOf op
    inferPrim env [e1, e2] typ

infer env (FnApplExpr expr (Tuple tuple)) = do
    (s1, t1) <- infer env expr
    inferPrim (apply s1 env) (map fixOrder tuple) t1
    where fixOrder (IndexedTElem expr) = expr
          fixOrder (NamedTElem _ expr) = expr -- FIXME: parser/fnAppl

infer env (EFn name prms rt body scope) = do
    tvars <- replicateM (length prms) fresh
    let ntp = zipWith (\p tv -> case p of
                         Param (BindPattern name) _ -> (name, Forall [] tv)
                         _                          -> error "zaxd") prms tvars
    let env' = env `extendTypeEnvAll` ntp
    (s1, t1) <- infer env' body
    let paramts = apply s1 tvars
    let rtype = foldr (:->) t1 paramts
    return (s1,  rtype)

-- ----------------------------------------------------------------------------
-- Runners
-- ----------------------------------------------------------------------------
runInfer :: InferM (Subst, IType) -> Either InferE Scheme
runInfer m = case evalState (runExceptT m) initInferS of
  Left err  -> Left err
  Right res -> Right $ closeOver res

inferExpr :: TypeEnv -> Expr -> Either InferE Scheme
inferExpr env = runInfer . infer env

inferModule :: TypeEnv -> Module -> Either InferE [(Name, Scheme)]
inferModule = undefined
