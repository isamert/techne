module Frontend.Infer
    ( -- Functions
      inferExpr
    , inferDecl
    , initTypeEnv
    , emptyTypeEnv
    -- Data
    , TypeEnv(..)
    , Subst(..)
    , IType(..)
    , Scheme(..)
    , TVar(..)
    , TCon(..)
    , Kind(..)
    -- Utils
    , generalize
    , pattern T
    , pattern Tv
    , pattern S
    , (->>)
    , pList
    , applyTuple
    , tVarA
    , tVarB
    , tVarC
    , tInt
    , tFloat
    , tBool
    , tChar
    , tFrac
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

pattern NotATuple = OtherError "Tuples can not have one element."

data InferE
    = UnboundVariable Text
    | UnificationFail IType IType
    | InfiniteType TVar IType
    | KindMismatch TVar IType
    | NotAnExpression Decl
    | OtherError Text
    deriving (Show, Eq)

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

-- A type variable with kind *
pattern Tv a = (TVar (TV a Star))

infixr -*>
infixr ->>
infixr `isKindOf`
infixr `TAp`

-- | Kind of a type like: Star -*> Star -*> Star
(-*>) :: Kind -> Kind -> Kind
a -*> b = KArr a b

-- | Function type like: tInt ->> tInt, synonymous for "->"
(->>) :: IType -> IType -> IType
a ->> b = TAp (TAp tArrow a) b

-- | Used while building type definitions with kinds, like: "[]" `isKindOf` Star -*> Star
isKindOf :: Text -> Kind -> IType
a `isKindOf` b = TCon (TC a b)

-- Type definitions
tVarA = TVar (TV "a" Star)
tVarB = TVar (TV "b" Star)
tVarC = TVar (TV "c" Star)

tBool  = T "bool"
tInt   = T "int"
tFloat = T "float"
tChar  = T "char"
tFrac  = T "frac"
tArrow = "->"  `isKindOf` Star -*> Star -*> Star
tList  = "[]"  `isKindOf` Star -*> Star

tTuple1 name = name `isKindOf` Star -*> Star
tTuple2 name = name `isKindOf` Star -*> Star -*> Star
tTuple3 name = name `isKindOf` Star -*> Star -*> Star -*> Star
tTuple4 name = name `isKindOf` Star -*> Star -*> Star -*> Star -*> Star
tTuple5 name = name `isKindOf` Star -*> Star -*> Star -*> Star -*> Star -*> Star
tTuple6 name = name `isKindOf` Star -*> Star -*> Star -*> Star -*> Star -*> Star -*> Star

-- Polymorphic types that you can apply a type, like [*]
pList                   = TAp tList
pDataType1 name a       = TAp (tTuple1 name) a
pDataType2 name a b     = TAp (TAp (tTuple2 name) a) b
pDataType3 name a b c   = TAp (TAp (tTuple3 name) a) (TAp b c)
pDataType4 name a b c d = TAp (TAp (tTuple4 name) a) (TAp b (TAp c d))

applyDataType :: Text -> [IType] -> IType
applyDataType name []               = name `isKindOf` Star
applyDataType name [x1]             = pDataType1 name x1
applyDataType name [x1, x2]         = pDataType2 name x1 x2
applyDataType name [x1, x2, x3]     = pDataType3 name x1 x2 x3
applyDataType name [x1, x2, x3, x4] = pDataType4 name x1 x2 x3 x4

-- ----------------------------------------------------------------------------
-- typeOf
-- ----------------------------------------------------------------------------
class Typed a where
    typeOf :: a -> IType

instance Typed Lit where
    typeOf (StrLit  _) = pList (tChar)
    typeOf (ChrLit  _) = tChar
    typeOf (IntLit  _) = tInt
    typeOf (FltLit  _) = tFloat
    typeOf (FracLit _) = tFrac

-- FIXME: implement type classes and get rid of different functions for Nums
initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv $ Map.fromList
    [ -- int functions:
      ("+", S $ tInt ->> tInt ->> tInt  )
    , ("-", S $ tInt ->> tInt ->> tInt  )
    , ("*", S $ tInt ->> tInt ->> tInt  )
    , ("/", S $ tInt ->> tInt ->> tFloat)

    -- float functions
    , ("+.", S $ tFloat ->> tFloat ->> tFloat)
    , ("-.", S $ tFloat ->> tFloat ->> tFloat)
    , ("*.", S $ tFloat ->> tFloat ->> tFloat)
    , ("/.", S $ tFloat ->> tFloat ->> tFloat)

    -- number utility
    , ("float2int", S $ tFloat ->> tInt  )
    , ("int2float", S $ tInt   ->> tFloat)

    -- list functions
    , ("++" , oneVarScheme $ pList (tVarA) ->> pList (tVarA)     ->> pList (tVarA))
    , ("map", twoVarScheme $ pList (tVarA) ->> (tVarA ->> tVarB) ->> pList (tVarB))

    -- generic functions
    , ("eq", oneVarScheme $ tVarA ->> tVarA ->> tBool)]

    where oneVarScheme = Forall [TV "a" Star]
          twoVarScheme = Forall [TV "a" Star, TV "b" Star]

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

filterNamedAndGeneralize  :: [(Maybe Name, IType)] -> [(Name, Scheme)]
filterNamedAndGeneralize xs =
    map (\(a,b) -> (fromJust a, Forall [] b))
      $ filter filterNamed xs

filterNamed  :: (Maybe Name, a) -> Bool
filterNamed (Just a, _) = True
filterNamed (Nothing, _) = False

--
-- Tuple stuff
--
-- FIXME: this should obey the order of the data type (if given)
-- Maybe this should be resolved earlier, like in the renamer or in a seperate
-- desugar phase
fixTupleOrder :: [TupleElem a] -> [a]
fixTupleOrder tuple = map extract tuple
    where extract (IndexedTElem x) = x
          extract (NamedTElem _ x) = x

applyTuple xs = flip applyDataType xs $
    case length xs of
      1 -> error "Tuples cannot have only one element"
      2 -> "(,)"
      3 -> "(,,)"
      4 -> "(,,,)"
      5 -> "(,,,,)"
      x -> error "sorry xd"

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

close :: IType -> Scheme
close ty = normalize $ generalize emptyTypeEnv ty

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

infer env (LitExpr lit) = return (emptySubst, typeOf lit)

infer env (EBinary opname e1 e2)  = do
    (_, typ) <- lookupEnv env opname
    inferPrim env [e1, e2] typ

infer env (EUnary opname e1) = do
    (_, typ) <- lookupEnv env opname
    inferPrim env [e1] typ

infer env (ETuple tup)
  | null tup || length tup == 1 = throwError NotATuple
  | otherwise = do
      let ordered = fixTupleOrder tup
      typs <- mapM (infer env) ordered
      let subs = foldr1 composeSubst (map fst typs)
      let returntyp = applyTuple (map (apply subs . snd) typs)
      return (subs, returntyp)

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
    (s2, t2) <- inferPrim (apply s1 env) (fixTupleOrder tuple) t1
    return (s2 `composeSubst` s1, t2)

infer env (WhenExpr cases) = do
    tv <- fresh Star
    foldM inferCase (emptySubst, tv) cases
    where inferCase (subst, r') (c, r) = do
            (s1, t1) <- infer env c
            (s2, t2) <- infer env r
            s3 <- unify t1 tBool
            s4 <- unify t2 r'
            return (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, apply s4 t2)

infer env (MatchExpr test cases) = do
    tv <- fresh Star
    (subst, testtyp) <- infer env test
    foldM (inferCase testtyp) (emptySubst, tv) cases
    where inferCase testtyp (subst, r') (ptrn, r) = do
            (pair@(name, t1), subtyps) <- inferPattern env ptrn
            (s2, t2) <- infer (env `extendTypeEnvAll` (filterNamedAndGeneralize (pair:subtyps))) r
            s3 <- unify t1 testtyp
            s4 <- unify t2 r'
            return (s4 `composeSubst` s3 `composeSubst` s2, apply s4 t2)

-- FIXME: (fn [[1], [a]] -> a) => can't infer a
infer env (EFn name prms rt body scope) = do
    p <- mapM (\(Param ptrn typ) -> inferPattern env ptrn) prms
    let paramtyps = concatMap (uncurry (:)) p
        env'      = env `extendTypeEnvAll` filterNamedAndGeneralize paramtyps
    (s1, t1) <- infer env' body
    let p2 = map fst p
        paramts = apply s1 (map snd p2)
    let rtype = foldr (->>) t1 paramts
    return (s1,  rtype)

inferPattern :: TypeEnv -> Pattern -> InferM ((Maybe Name, IType), [(Maybe Name, IType)])
inferPattern _ (BindPattern name) =
  fresh Star >>= \tvar -> return ((Just name, tvar), [])

inferPattern _ (ElsePattern name) =
  fresh Star >>= \tvar -> return ((name, tvar),[])

inferPattern _ (RestPattern name) =
  fresh Star >>= \tvar -> return ((name, tvar), [])

inferPattern _ (RegexPattern name _) =
  fresh Star >>= \tvar -> return ((name, T"string"), [])

inferPattern _ (LitPattern name lit) = do
    (s, t) <- infer emptyTypeEnv (LitExpr lit)
    return ((name, t), [])

inferPattern env (TuplePattern name (Tuple t))
  | length t < 2 = throwError NotATuple
  | otherwise = do
      let ordered = fixTupleOrder t
      i <- mapM (inferPattern env) ordered
      let returntyp = applyTuple $ map (snd . fst) i
      return ((name, returntyp), concatMap (uncurry (:)) i)

inferPattern env (ListPattern name (List l)) -- from now on, it's just a shitshow (and still not working properly) :(
  | null l = fresh Star >>= \t -> return ((name, pList t), [])
  | otherwise = do
      firstp@((_, firstelemtyp), subtyps) <- inferPattern env (head l)
      p <- mapM (inferPattern env) (tail l)
      (s,t) <- foldrM (\((x,t'),y) (subst, t) -> do
          s <- unify t t'
          return (s `composeSubst` subst, apply s t'))
          (emptySubst, firstelemtyp) (firstp:p)
      let unifiedtyps = map (\((x,t'),y) -> ((x, apply s t'),y)) (firstp:p)
          unifiedfixed = case last l of
                   (RestPattern name) -> init unifiedtyps ++ [(\((x,t'),y) -> ((x, pList t'), y)) (last unifiedtyps)]
                   _ -> unifiedtyps
          alltyps     = concatMap (uncurry (:)) unifiedfixed ++ subtyps
      return ((name, pList t), alltyps)

inferPattern env (UnpackPattern name typname (Tuple tuple)) = do
    tv <- fresh Star
    (s1, t1) <- lookupEnv env typname
    (t', subtyps) <- foldM inferStep (id, []) (fixTupleOrder tuple)
    s2 <- unify (t' tv) t1
    let rettype = apply s2 tv -- FIXME: check if fully applied somehow
    return ((name, rettype), subtyps)
    where inferStep (t, subptrns) exp = do
            (ptrn@(_, t'), subptrns') <- inferPattern env exp
            return (t . (t' ->>), (ptrn:subptrns) ++ subptrns')

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

inferDecl env (DataDecl dat@(Dat name vars datapairs)) =
    Right $ extendTypeEnvAll env constructorTypes
    where constructorTypes = map (inferDataCons name vars) datapairs

inferDecl _ decl = Left $ NotAnExpression decl

-- FIXME: this only produces rank-1 kind. To produce rank-n kinds
-- I probably need to look inside the data definition and infer from there.
inferDataCons typname typvars (name, params) =
    (name, close $ foldr (\param typ -> param2type param ->> typ) (returnType typvars)  params)
        where param2type (DataParam paramname (ConcreteType typname)) = T typname
              param2type (DataParam paramname (ConstraintType typname)) = Tv typname
              nameOfConstraint (TypeConstraint name) = name

              returnType xs = applyDataType typname (map (Tv . nameOfConstraint) xs)
