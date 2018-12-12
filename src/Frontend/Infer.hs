module Frontend.Infer
    ( inferExpr
    , inferDecl
    , initTypeEnv
    , TypeEnv(..)
    , Subst(..)
    , IType(..)
    , Scheme(..)
    , TVar(..)
    , TCon(..)
    , Kind(..)
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

-- TODO: document functions :(
-- TODO: typecheck user data types and patterns
-- TODO: refactor AST with IType and Kind

-- ----------------------------------------------------------------------------
-- Definitions
-- ----------------------------------------------------------------------------
data Scheme = Forall [TVar] IType    deriving (Show, Eq, Ord)
data Kind   = Star | KArr Kind Kind  deriving (Show, Eq, Ord)
data TVar   = TV Text Kind           deriving (Show, Eq, Ord)
data TCon   = TC Text Kind           deriving (Show, Eq, Ord)

-- | An (i)nferred type
data IType
    = TVar TVar
    | TCon TCon
    | TAp  IType IType
    deriving (Show, Eq, Ord)

data InferE
    = UnboundVariable Text
    | UnificationFail IType IType
    | InfiniteType TVar IType
    | KindMismatch TVar IType
    deriving (Show, Eq, Ord)

newtype TypeEnv = TypeEnv (Map.Map Name Scheme) deriving (Show, Eq, Ord)
newtype InferS = InferS { counter :: Int }
type InferM = ExceptT InferE (State InferS)
type Subst = Map.Map TVar IType

-- ----------------------------------------------------------------------------
-- Type utilities
-- ----------------------------------------------------------------------------
-- A concrete type
pattern T a = TCon (TC a Star)

-- A type scheme for a concrete type
pattern S a = Forall [] a

pattern Tv a = (TVar (TV a Star))

infixr ->>
infixr -*>
infixr `isKindOf`
infixr `TAp`

-- | Kind of a type like: Star -*> Star -*> Star
(-*>) :: Kind -> Kind -> Kind
a -*> b = KArr a b

-- | Function type like: T"int" ->> T"int", synonymous for "->"
(->>) :: IType -> IType -> IType
a ->> b = TAp (TAp tArrow a) b

-- | Used while building type definitions with kinds, like: "[]" `isKindOf` Star -*> Star
isKindOf :: Text -> Kind -> IType
a `isKindOf` b = TCon (TC a b)

-- Type definitions
tArrow      = "->"      `isKindOf` Star -*> Star -*> Star
tList       = "[]"      `isKindOf` Star -*> Star
tTuple2     = "(,)"     `isKindOf` Star -*> Star -*> Star
tTuple3     = "(,,)"    `isKindOf` Star -*> Star -*> Star -*> Star
tTuple4     = "(,,,)"   `isKindOf` Star -*> Star -*> Star -*> Star -*> Star
tTuple5     = "(,,,,)"  `isKindOf` Star -*> Star -*> Star -*> Star -*> Star -*> Star
tTuple6     = "(,,,,,)" `isKindOf` Star -*> Star -*> Star -*> Star -*> Star -*> Star -*> Star

-- Polymorphic types that you can apply a type, like [*]
pArrow       = TAp tArrow
pList        = TAp tList
pTuple2      = TAp tTuple2
pTuple3      = TAp tTuple3
pTuple4      = TAp tTuple4
pTuple5      = TAp tTuple5
pTuple6      = TAp tTuple6

-- An example for (int,int):
-- y = pTuple2 (T"int" `TAp` T"int")

-- ----------------------------------------------------------------------------
-- typeOf
-- ----------------------------------------------------------------------------
class Typed a where
    typeOf :: a -> InferM IType

-- FIXME: implement type classes and get rid of different functions for Nums
instance Typed Op where
    -- int ops
    typeOf (BinOp   "+")  = return $ T"int" ->> T"int" ->> T"int"
    typeOf (BinOp   "-")  = return $ T"int" ->> T"int" ->> T"int"
    typeOf (BinOp   "*")  = return $ T"int" ->> T"int" ->> T"int"
    typeOf (BinOp   "/")  = return $ T"int" ->> T"int" ->> T"float"

    -- float ops
    typeOf (BinOp   "+.")  = return $ T"float" ->> T"float" ->> T"float"
    typeOf (BinOp   "-.")  = return $ T"float" ->> T"float" ->> T"float"
    typeOf (BinOp   "*.")  = return $ T"float" ->> T"float" ->> T"float"
    typeOf (BinOp   "/.")  = return $ T"float" ->> T"float" ->> T"float"

    -- list ops
    typeOf (BinOp  "++") =
        instantiate $ Forall [TV "a" Star] (gList ->> gList ->> gList)
        where gList = pList (TVar (TV "a" Star))

instance Typed Lit where
    typeOf (StrLit  _) = return $ pList (T"char")
    typeOf (ChrLit  _) = return $ T"char"
    typeOf (IntLit  _) = return $ T"int"
    typeOf (FltLit  _) = return $ T"float"
    typeOf (FracLit _) = return $ T"frac"
    typeOf (BoolLit _) = return $ T"bool"

initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv $ Map.fromList
    [ ("float2int", S $ T"float" ->> T"int")
    , ("int2float", S $ T"int" ->> T"float")
    , ("map",       Forall [TV "a" Star, TV "b" Star] (pList (Tv"a") ->> (Tv"a" ->> Tv"b") ->> pList (Tv"b")))]

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

-- ----------------------------------------------------------------------------
-- kindOf
-- ----------------------------------------------------------------------------
class Kinded t where
    kindOf :: t -> Kind

instance Kinded TVar where
    kindOf (TV v k) = k

instance Kinded TCon where
    kindOf (TC v k) = k

instance Kinded IType where
    kindOf (TCon tc) = kindOf tc
    kindOf (TVar u)  = kindOf u
    kindOf (TAp t _) = case kindOf t of
                         (KArr _ k) -> k

-- ----------------------------------------------------------------------------
-- Substitutable
-- ----------------------------------------------------------------------------
class Substitutable a where
    apply :: Subst -> a -> a    -- apply the Subst over expressions
    ftv   :: a -> Set.Set TVar  -- query free variables and return them

instance Substitutable IType where
    apply _ (TCon a)       = TCon a
    apply s t@(TVar a)     = Map.findWithDefault t a s
    apply s (t1 `TAp` t2)  = apply s t1 `TAp` apply s t2

    ftv (TVar var) = Set.singleton var
    ftv (TCon _  ) = Set.empty
    ftv (t1 `TAp` t2) = ftv t1 `Set.union` ftv t2

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
-- Helpers
-- ----------------------------------------------------------------------------
initInferS :: InferS
initInferS = InferS 0

extendTypeEnv :: TypeEnv -> (Name, Scheme) -> TypeEnv
extendTypeEnv (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

extendTypeEnvAll :: TypeEnv -> [(Name, Scheme)] -> TypeEnv
extendTypeEnvAll (TypeEnv env) tsps = TypeEnv $ Map.fromList tsps `Map.union` env

fresh :: Kind -> InferM IType
fresh k = do
  s <- get
  put s{counter = counter s + 1}
  return $ TVar $ TV ("t" ++ tshow (counter s)) k

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

emptySubst :: Subst
emptySubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Only used while normalizing.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']
-- ----------------------------------------------------------------------------
-- Main functions
-- ----------------------------------------------------------------------------
unify ::  IType -> IType -> InferM Subst
unify (l `TAp` r) (l' `TAp` r')  = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `composeSubst` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return emptySubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> IType -> InferM Subst
bind a t | t == TVar a          = return emptySubst
         | occursCheck a t      = throwError $ InfiniteType a t
         | kindOf a /= kindOf t = throwError $ KindMismatch a t
         | otherwise            = return $ Map.singleton a t

instantiate ::  Scheme -> InferM IType
instantiate (Forall as t) = do
  as' <- mapM (\(TV _ k) -> fresh k) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> IType -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

closeOver :: (Subst, IType) -> Scheme
closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTypeEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where ord = map (\(TV a k, TV a' k') -> (TV a k, TV a' k)) $ zip fvs (fmap (flip TV Star . tpack) letters)
        fvs = nub $ fv body

        fv (TVar a)   = [a]
        fv (a `TAp` b)  = fv a ++ fv b
        fv (TCon _)   = []

        normtype (a `TAp` b) = normtype a `TAp` normtype b
        normtype (TCon a)    = TCon a
        normtype (TVar a)    =
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
    tv <- fresh Star
    (s1, tf) <- foldM inferStep (emptySubst, id) l
    s2 <- unify (apply s1 (tf tv)) t
    return (s2 `composeSubst` s1, apply s2 tv)
    where inferStep (s, tf) exp = do
            (s', t) <- infer (apply s env) exp
            return (s' `composeSubst` s, tf . (t ->>))

infer :: TypeEnv -> Expr -> InferM (Subst, IType)
infer env (ERef name _) =  lookupEnv env name

infer env (LitExpr lit)   = do
    typ <- typeOf lit
    return (emptySubst, typ)

infer env (BinExpr op e1 e2)   = do
    typ <- typeOf op
    inferPrim env [e1, e2] typ

infer env (UnExpr op e1)   = do
    typ <- typeOf op
    inferPrim env [e1] typ

infer env (ETuple tup)
  | null tup || length tup == 1 = error "The fuck?"
  | otherwise = do
      let ordered = map fixOrder tup
      i <- mapM (infer env) ordered
      let app = foldr1 TAp (map snd i)
      let subs = foldr1 composeSubst (map fst i)
      return (subs, selectTupleCons ordered app)
    where fixOrder (IndexedTElem expr) = expr
          fixOrder (NamedTElem _ expr) = expr
          selectTupleCons tup = case length tup of
                                  2 -> pTuple2
                                  3 -> pTuple3
                                  4 -> pTuple4
                                  5 -> pTuple5
                                  6 -> pTuple6

infer env (EList l)
  | null l = fresh Star >>= \t -> return (emptySubst, pList t)
  | otherwise = do
      i <- infer env (head l)
      p <- mapM (infer env) (tail l)
      (s, t) <- foldrM unifyList i p
      return (s, apply s (pList t))
    where unifyList (s, t) (s', t') = do
            s2 <- unify t' t
            return (s2 `composeSubst` s' `composeSubst` s, apply s2 t)

infer env (FnApplExpr expr (Tuple tuple)) = do
    (s1, t1) <- infer env expr
    (s2, t2) <- inferPrim (apply s1 env) (map fixOrder tuple) t1
    return $ (s2 `composeSubst` s1, t2)
    where fixOrder (IndexedTElem expr) = expr
          fixOrder (NamedTElem _ expr) = expr -- FIXME: parser/fnAppl

infer env (EFn name prms rt body scope) = do
    ps <- mapM (\(Param ptrn typ) -> inferPattern ptrn) prms
    let paramtyps = concat ps
        env'      = env `extendTypeEnvAll` (onlyNamed paramtyps)
    (s1, t1) <- infer env' body
    let paramts = apply s1 (map (scheme2itype . snd) paramtyps)
    let rtype = foldr (->>) t1 paramts
    return (s1,  rtype)
    where inferPattern (BindPattern name) = do
            fresh Star >>= \tvar -> return [(Just name, Forall [] tvar)]
          inferPattern (ElsePattern name) = do
            fresh Star >>= \tvar -> return [(name, Forall [] tvar)]
          inferPattern (RestPattern name) = do
            fresh Star >>= \tvar -> return [(name, Forall [] (pList tvar))]
          inferPattern (RegexPattern name _) = do
            fresh Star >>= \tvar -> return [(name, Forall [] (T"string"))]
          inferPattern (LitPattern name lit) = do
              (s, t) <- infer emptyTypeEnv (LitExpr lit)
              return [(name, generalize emptyTypeEnv t)]
          inferPattern (ListPattern name (List l))
            | null l = fresh Star >>= \t -> return [(name, Forall [] (pList t))]
            {-| otherwise = do
                i <- inferPattern emptyTypeEnv (head l)
                p <- mapM (inferPattern emptyTypeEnv) (tail l)
                (s, t) <- foldrM unifyList i p
                return (name, apply s (pList t))
              where unifyList (_, t) (_, t') = do
                      s2 <- unify t' t
                      return (, apply s2 t)
            -}

          scheme2itype (Forall _ itype) = itype

          onlyNamed xs = map (\(a,b) -> (fromJust a, b)) $ filter filterNameless xs
          filterNameless (Just a, _) = True
          filterNameless (Nothing, _) = False



-- ----------------------------------------------------------------------------
-- Runners
-- ----------------------------------------------------------------------------
runInfer :: InferM (Subst, IType) -> Either InferE Scheme
runInfer m = case evalState (runExceptT m) initInferS of
  Left err  -> Left err
  Right res -> Right $ closeOver res

inferExpr :: TypeEnv -> Expr -> Either InferE Scheme
inferExpr env = runInfer . infer env

inferModule :: TypeEnv -> Module -> Either InferE TypeEnv
inferModule = undefined

inferDecl :: TypeEnv -> Decl -> Either InferE TypeEnv
inferDecl env (FnDecl fn@(Fn (Just name) _ _ _ _)) =
    (\scheme -> extendTypeEnv env (name,scheme)) <$> inferExpr env (FnExpr fn)
