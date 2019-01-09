module Infer
    ( -- Functions
      inferExpr
    , inferDecl
    , inferModule
    , initTypeEnv
    , emptyTypeEnv
    -- Data
    , TypeEnv(..)
    , Subst(..)
    -- Utils
    , generalize
    , close
    , pList
    , applyTuple
    , applyDataType
    ) where

import TechnePrelude
import Err
import Syntax

import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
import qualified Data.Set as Set

--  Adaptation of: http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- FIXME: generate tuples and datatypes programatically

-- ----------------------------------------------------------------------------
-- Definitions
-- ----------------------------------------------------------------------------

pattern NotATuple = InferenceErr (OtherError "Tuples can not have one element.")

newtype TypeEnv = TypeEnv (Map.Map Name Scheme) deriving (Show, Eq, Ord)
newtype InferS = InferS { counter :: Int }
type InferM = TechneM (State InferS)
type Subst = Map.Map TVar Type

-- ----------------------------------------------------------------------------
-- Type utilities
-- ----------------------------------------------------------------------------

tDataType1 c name = c name $ Star :-*> Star
tDataType2 c name = c name $ Star :-*> Star :-*> Star
tDataType3 c name = c name $ Star :-*> Star :-*> Star :-*> Star
tDataType4 c name = c name $ Star :-*> Star :-*> Star :-*> Star :-*> Star
tDataType5 c name = c name $ Star :-*> Star :-*> Star :-*> Star :-*> Star :-*> Star
tDataType6 c name = c name $ Star :-*> Star :-*> Star :-*> Star :-*> Star :-*> Star :-*> Star

-- Polymorphic types that you can apply a type, like [*]
pList                       = TAp TList
pDataType1 cns name a       = TAp (tDataType1 cns name) a
pDataType2 cns name a b     = TAp (TAp (tDataType2 cns name) a) b
pDataType3 cns name a b c   = TAp (TAp (tDataType3 cns name) a) (TAp b c)
pDataType4 cns name a b c d = TAp (TAp (tDataType4 cns name) a) (TAp b (TAp c d))

applyDataType :: (Name -> Kind -> Type) -> Text -> [Type] -> Type
applyDataType c name []               = c name Star
applyDataType c name [x1]             = pDataType1 c name x1
applyDataType c name [x1, x2]         = pDataType2 c name x1 x2
applyDataType c name [x1, x2, x3]     = pDataType3 c name x1 x2 x3
applyDataType c name [x1, x2, x3, x4] = pDataType4 c name x1 x2 x3 x4

-- ----------------------------------------------------------------------------
-- typeOf
-- ----------------------------------------------------------------------------

class Typed a where
    typeOf :: a -> Type

instance Typed Lit where
    typeOf (StrLit  _) = pList TChar
    typeOf (ChrLit  _) = TChar
    typeOf (IntLit  _) = TInt
    typeOf (FltLit  _) = TFloat
    typeOf (FracLit _) = TFrac

-- FIXME: implement type classes and get rid of different functions for Nums
initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv $ Map.fromList
    [ -- int functions:
      ("internalSum", S $ TInt :->> TInt :->> TInt  )
    , ("internalSub", S $ TInt :->> TInt :->> TInt  )
    , ("internalMul", S $ TInt :->> TInt :->> TInt  )
    , ("internalDiv", S $ TInt :->> TInt :->> TFloat)

     -- list functions
    , ("internalArrPrep" , oneVarScheme $ TVarA :->> pList TVarA :->> pList TVarA)
    , ("internalArrJoin" , oneVarScheme $ pList TVarA :->> pList TVarA :->> pList TVarA)

    ]

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

instance Kinded Type where
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

instance Substitutable Type where
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

unionTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
unionTypeEnv (TypeEnv env1) (TypeEnv env2) = TypeEnv $ Map.union env1 env2

extendTypeEnv :: TypeEnv -> (Name, Scheme) -> TypeEnv
extendTypeEnv (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

extendTypeEnvAll :: TypeEnv -> [(Name, Scheme)] -> TypeEnv
extendTypeEnvAll (TypeEnv env) tsps = TypeEnv $ Map.fromList tsps `Map.union` env

fresh :: Kind -> InferM Type
fresh k = do
  s <- get
  put s{counter = counter s + 1}
  return $ TVar $ TV ("tv" ++ tshow (counter s)) k

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

emptySubst :: Subst
emptySubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Only used while normalizing.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

filterNamedAndGeneralize  :: [(Maybe Name, Type)] -> [(Name, Scheme)]
filterNamedAndGeneralize xs =
    map (\(a,b) -> (fromJust a, Forall [] b))
      $ filter filterNamed xs

filterNamed  :: (Maybe Name, a) -> Bool
filterNamed (Just a, _) = True
filterNamed (Nothing, _) = False

--
-- Tuple stuff
--

applyTuple xs = flip (applyDataType TyCon) xs $
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

unify ::  Type -> Type -> InferM Subst
unify (l `TAp` r) (l' `TAp` r')  = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `composeSubst` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return emptySubst
unify t1 t2 = throwError $ InferenceErr $ UnificationFail t1 t2

bind ::  TVar -> Type -> InferM Subst
bind a t | t == TVar a          = return emptySubst
         | occursCheck a t      = throwError $ InferenceErr $ InfiniteType a t
         | kindOf a /= kindOf t = throwError $ InferenceErr $ KindMismatch a t
         | otherwise            = return $ Map.singleton a t

instantiate ::  Scheme -> InferM Type
instantiate (Forall as t) = do
  as' <- mapM (\(TV _ k) -> fresh k) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

close :: Type -> Scheme
close ty = normalize $ generalize emptyTypeEnv ty

closeOver :: (Subst, Type) -> Scheme
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

lookupEnv :: TypeEnv -> Name -> InferM (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ InferenceErr $ UnboundVariable (tshow x)
    Just s  -> do t <- instantiate s
                  return (emptySubst, t)

inferPrim :: TypeEnv -> [Expr] -> Type -> InferM (Subst, Type)
inferPrim env l t = do
    tv <- fresh Star
    (s1, tf) <- foldM inferStep (emptySubst, id) l
    s2 <- unify (apply s1 (tf tv)) t
    return (s2 `composeSubst` s1, apply s2 tv)
    where inferStep (s, tf) exp = do
            (s', t) <- infer (apply s env) exp
            return (s' `composeSubst` s, tf . (t :->>))

infer :: TypeEnv -> Expr -> InferM (Subst, Type)
infer env (ERef name) =  lookupEnv env name

infer env (LitExpr lit) = return (emptySubst, typeOf lit)

infer env (BinExpr opname e1 e2)  = do
    (_, typ) <- lookupEnv env opname
    inferPrim env [e1, e2] typ

infer env (UnExpr opname e1) = do
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

infer env (FixExpr e1) = do
    tv <- fresh Star
    inferPrim env [e1] ((tv :->> tv) :->> tv)

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
            s3 <- unify (apply s2 t1) TBool
            s4 <- unify (apply s3 t2) r'
            return (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
                   , apply (s4 `composeSubst` s3 `composeSubst` s1) t2)

infer env (MatchExpr test cases) = do
    tv <- fresh Star
    (subst, testtyp) <- infer env test
    (s, testtyp, rtype) <- foldM inferCase (subst, testtyp, tv) cases
    return (s, rtype)
    where inferCase (subst, testtyp, exprtyp') (ptrn, exprtyp) = do
            (pair@(name, t1), subtyps) <- inferPattern env ptrn
            (s2, t2) <- infer (env `extendTypeEnvAll` filterNamedAndGeneralize (pair:subtyps)) exprtyp
            s3 <- unify (apply s2 t1) testtyp
            s4 <- unify (apply s3 t2) exprtyp'
            return (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` subst
                   , apply s2 t1
                   , apply (s4 `composeSubst` s3) t2)

infer env (EFn name prms body scope) = do
    inferedptrns <- mapM (inferPattern env . paramPtrn) prms
    let allinferedtyps = concatMap (uncurry (:)) inferedptrns
        env' = env `extendTypeEnvAll` filterNamedAndGeneralize allinferedtyps
    (s1, t1) <- infer env' body
    let nametyppair = map fst inferedptrns
        paramtyps   = apply s1 (map snd nametyppair)
    let returntype  = foldr (:->>) t1 paramtyps
    return (s1,  returntype)

-- FIXME: (fn [[1], [a]] -> a) => can't infer a
inferPattern :: TypeEnv -> Pattern -> InferM ((Maybe Name, Type), [(Maybe Name, Type)])
inferPattern _ (BindPattern name (Just scheme)) = do
    t <- instantiate scheme
    return ((Just name, t), []) -- FIXME: check if type exists and properly applied
inferPattern _ (BindPattern name typ) =
  fresh Star >>= \tvar -> return ((Just name, tvar), [])

inferPattern _ (ElsePattern name) =
  fresh Star >>= \tvar -> return ((name, tvar),[])

inferPattern _ (RestPattern name) =
  fresh Star >>= \tvar -> return ((name, tvar), [])

inferPattern _ (RegexPattern name _) =
  fresh Star >>= \tvar -> return ((name, pList $ T"char"), [])

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
            return (t . (t' :->>), (ptrn:subptrns) ++ subptrns')

-- ----------------------------------------------------------------------------
-- Runners
-- ----------------------------------------------------------------------------

runInferM m = evalState (runExceptT m) initInferS

runInfer :: InferM (Subst, Type) -> TechneResult Scheme
runInfer m = case runInferM m of
  Left err  -> Left err
  Right res -> Right $ closeOver res

inferExpr :: TypeEnv -> Expr -> TechneResult Scheme
inferExpr env = runInfer . infer env

inferModule :: TypeEnv -> Module -> TechneResult TypeEnv
inferModule env (Module imports []) = Right env
inferModule env (Module imports (decl:decls)) =
    case inferDecl env decl of
      Right e -> let nenv = unionTypeEnv e env in
                 inferModule nenv (Module imports decls)
      Left y -> Left y

inferDecl :: TypeEnv -> Decl -> TechneResult TypeEnv
inferDecl env (FnDecl fn@(Fn (Just name) _ _ _)) =
    (\scheme -> extendTypeEnv env (name,scheme)) <$> inferExpr env (FnExpr fn)

inferDecl env (DataDecl dat@(Dat name vars datapairs)) =
  case runInferM constructorTypes of
    Left err  -> Left err
    Right res -> Right $ env `extendTypeEnvAll` res
    where constructorTypes = concat <$> mapM (inferDataCons env name vars) datapairs

inferDecl _ decl = Left $ InferenceErr $ NotAnExpression decl

-- FIXME: this only produces rank-1 kind. To produce rank-n kinds
-- I probably need to look inside the data definition and infer from there.
-- FIXME: do not add type itself to type environment use seperate TypeConsEnv?
--        and also check if they exist in param2type
inferDataCons :: TypeEnv -> Name -> [Constraint] -> (Name, [DataParam]) -> InferM [(Name, Scheme)]
inferDataCons env typname typvars (consname, consparams) = do
    ctyp <- constyp
    accessors <- mapM mkFieldAccessorFn consparams
    return $ (typname, close returnType):(consname, close ctyp):accessors
        where --FIXME: check if type exists and properly applied
              param2type (DataParam _ typ) = return typ
              nameOfConstraint (TypeConstraint name) = name
              constyp = foldrM (\param typ -> do
                                                prmtyp <- param2type param
                                                return $ prmtyp :->> typ) returnType consparams

              returnType = applyDataType TyCon typname (map (Tv . nameOfConstraint) typvars)
              mkFieldAccessorFn (DataParam fieldname fieldtyp) = do
                  return (typname ++ FieldAccessor ++ fieldname,
                          close $ returnType :->> fieldtyp)
