{-# Language LambdaCase #-}
{-# Language TypeSynonymInstances #-}
{-# Language TupleSections #-}

module Infer (typecheck, generalize) where


import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS

import Data.List
import Data.Maybe
import Data.Functor
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

import AST
import Type
import Kind

data TypeError = Mismatch Type Type | NotFunction Type | NotDefined String | NotDefinedMany [TVar] | Redefinition String
               | EmptyBlock | GlobalPass | BlockData | EmptyMatch | NotExhaustive [String]
               | UnifyError [Type] [Type] | InfiniteType TVar Type deriving (Show)

type Env = Map.Map String Scheme
type DMap = Map.Map String [String]
type Matches = [(Type, [String])]
type Infer a = RWST (Env, DMap) [Constraint] (Int, Matches) (Except TypeError) a
type Solver a = ExceptT TypeError Identity a
type Subst = Map.Map TVar Type

-----------------------------
-- Substitutable Typeclass --
-----------------------------

class Substitutable a where
    tvs :: a -> Set.Set TVar
    apply :: Subst -> a -> a

instance Substitutable Type where
    tvs (TVar tv) = Set.singleton tv
    tvs (TCon _ ts) = foldr (Set.union . tvs) Set.empty ts
    apply s t@(TVar tv) = Map.findWithDefault t tv s
    apply s (TCon c ts) = TCon c $ map (apply s) ts

instance Substitutable Scheme where
    tvs (Forall vs t) = tvs t `Set.difference` vs
    apply s (Forall vs t) = Forall vs $ apply (foldr Map.delete s vs) t

instance Substitutable Constraint where
    tvs (t1 :~ t2) = tvs t1 `Set.union` tvs t2
    apply s (t1 :~ t2) = apply s t1 :~ apply s t2

instance Substitutable a => Substitutable [a] where
    tvs l = foldr (Set.union . tvs) Set.empty l
    apply s = map (apply s)

instance Substitutable a => Substitutable (Decl a) where
    tvs = error "tvs called on decl"
    apply s = \case
        DVar tM id e -> DVar tM id (apply s e)
        DStmt st -> DStmt (apply s st)
        DData {} -> error "attempt to substitute data decl"

instance Substitutable a => Substitutable (Stmt a) where
    tvs = error "tvs called on stmt"
    apply s = \case
        SExpr e -> SExpr (apply s e)
        SPass e -> SPass (apply s e)

instance Substitutable a => Substitutable (Expr a) where
    tvs = error "tvs called on expr"
    apply s = \case
        EBlock t ds -> EBlock (apply s t) $ map (apply s) ds
        EAssign t l r -> EAssign (apply s t) (apply s l) (apply s r)
        EMatch t e bs -> let (ps, bes) = unzip bs in EMatch (apply s t) (apply s e) (zip ps (map (apply s) bes))
        EIf t c a b -> EIf (apply s t) (apply s c) (apply s a) (apply s b)
        ECall t f a -> ECall (apply s t) (apply s f) (apply s a)
        EBinary t o l r -> EBinary (apply s t) o (apply s l) (apply s r)
        EUnary t o a -> EUnary (apply s t) o (apply s a)
        EIdent t id -> EIdent (apply s t) id
        EString t st -> EString (apply s t) st
        EBool t b -> EBool (apply s t) b
        EInt t i -> EInt (apply s t) i
        EFloat t f -> EFloat (apply s t) f
        EUnit t -> EUnit (apply s t)
        EFunc t pM rM p e -> EFunc (apply s t) pM rM p (apply s e)

---------------------
-- Misc. Functions --
---------------------

fresh :: Infer Type
fresh = do
    (n, ms) <- get
    put (n+1, ms)
    return . TVar . TV $ (varNames !! n)
    where varNames = map ('$':) $ [1..] >>= flip replicateM ['a'..'z']

constrain :: Constraint -> Infer ()
constrain = tell . (:[])

addMatch :: (Type, [String]) -> Infer ()
addMatch m = do
    (n, ms) <- get
    put (n, m:ms)

constrainIf :: (Maybe Type, Type) -> Infer ()
constrainIf (a, b) = case a of
    Just t -> constrain $ t :~ b
    Nothing -> return ()

compose :: Subst -> Subst -> Subst
compose a b = Map.map (apply a) b `Map.union` a

generalize :: Env -> Type -> Scheme
generalize env t = Forall vs t
    where vs = tvs t `Set.difference` tvs (Map.elems env)

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
    let vs' = Set.toList vs
    nvs <- mapM (const fresh) vs'
    let s = Map.fromList (zip vs' nvs)
    return $ apply s t

askEnv :: Infer Env
askEnv = do
    (env, _) <- ask
    return env

-----------------
-- Unification --
-----------------

unify :: Type -> Type -> Solver Subst
unify a b | a == b = return Map.empty
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify a@(TCon c1 ts1) b@(TCon c2 ts2)
    | c1 /= c2 = throwError $ Mismatch a b
    | otherwise = unifyMany ts1 ts2

unifyMany :: [Type] -> [Type] -> Solver Subst
unifyMany [] [] = return Map.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnifyError t1 t2

bind :: TVar -> Type -> Solver Subst
bind v t
    | v `Set.member` tvs t = throwError $ InfiniteType v t
    | otherwise = return $ Map.singleton v t 

solve :: Subst -> [Constraint] -> Solver Subst
solve s c =
    case c of
        [] -> return s
        ((t1 :~ t2) : cs) -> do
            s1 <- unify t1 t2
            solve (s1 `compose` s) (apply s1 cs)

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solve Map.empty cs

typecheck :: UntypedProgram -> Either TypeError String
typecheck decls = case runIdentity $ runExceptT $ runRWST (inferProgram decls []) (Map.empty, Map.empty) (0, []) of
    Left err -> Left err
    Right ((p, dmap), (_, ms), cs) -> do
        s <- runSolve cs
        checkMatches ms dmap s
        return $ show (apply s p)

exhaustiveCheck :: DMap -> (String, [String]) -> Either TypeError ()
exhaustiveCheck dmap (tc, bs) = do
    case Map.lookup tc dmap of
        Nothing -> Left $ NotDefined tc
        Just vns -> let bs' = Set.fromList bs ; vns' = Set.fromList vns
                    in when (bs' /= vns') (Left $ NotExhaustive (Set.toList (vns' `Set.difference` bs')))

checkMatches :: Matches -> DMap -> Subst -> Either TypeError ()
checkMatches ms dmap s = do
    let ms' = map (\(t, as) -> let mt = apply s t in let (TCon tc _) = mt in (tc, as)) ms
    mapM_ (exhaustiveCheck dmap) ms'

--------------------
-- Operator Types --
--------------------

bOpType :: Type -> Type -> BinOp -> Type
bOpType l r = \case
    Or -> TBool `TFunc` (TBool `TFunc` TBool)
    And -> TBool `TFunc` (TBool `TFunc` TBool)
    NotEqual -> l `TFunc` (r `TFunc` TBool)
    Equal -> l `TFunc` (r `TFunc` TBool)
    GreaterEqual -> TInt `TFunc` (TInt `TFunc` TBool)
    Greater -> TInt `TFunc` (TInt `TFunc` TBool)
    LesserEqual -> TInt `TFunc` (TInt `TFunc` TBool)
    Lesser -> TInt `TFunc` (TInt `TFunc` TBool)
    Add -> TInt `TFunc` (TInt `TFunc` TInt)
    Sub -> TInt `TFunc` (TInt `TFunc` TInt)
    Mul -> TInt `TFunc` (TInt `TFunc` TInt)
    Div -> TInt `TFunc` (TInt `TFunc` TInt)
    Exp -> TInt `TFunc` (TInt `TFunc` TInt)

uOpType :: Type -> UnaOp -> Type
uOpType _ = \case
    Neg -> TInt `TFunc` TInt
    Not -> TBool `TFunc` TBool

-----------------------
-- Build Constraints --
-----------------------

scoped :: String -> Scheme -> Infer a -> Infer a
scoped x sc m = do
    let scope (e, d) = (Map.insert x sc (Map.delete x e), d)
    local scope m

scopedMany :: [String] -> [Scheme] -> Infer a -> Infer a
scopedMany (x : xs) (sc : scs) m = scoped x sc (scopedMany xs scs m)
scopedMany _ _ m = m

inferProgram :: UntypedProgram -> [TypedDecl] -> Infer (TypedProgram, DMap)
inferProgram [] tds = do
    (_, dmap) <- ask
    return (reverse tds, dmap)
inferProgram (d : ds) tds =
    case d of
        DStmt s -> inferStmt s >>= \s' -> inferProgram ds (DStmt s' : tds)
        DData tc tps vcs -> createValueConsts tc tps vcs (inferProgram ds tds)
        DVar _ id _ -> inferVarDecl d >>= \(td', sc) -> scoped id sc (inferProgram ds (td' : tds))

-----------Constructors
createValueConsts :: String -> [Type] -> [(String, [Type])] -> Infer a -> Infer a
createValueConsts _ _ [] n = n
createValueConsts tc tps ((vn, vts) : vcs) n = do
    env <- askEnv
    let vtps = tvs tps
    let vvts = tvs vts
    if (vtps `Set.intersection` vvts) /= vvts
        then throwError $ NotDefinedMany (Set.toList (vvts `Set.difference` vtps))
        else case vts of
            [] -> let sc = (generalize env $ TCon tc tps) ; scope (e, d) = (Map.insert vn sc (Map.delete vn e), Map.insert tc (vn : Map.findWithDefault [] tc d) d)
                  in local scope (createValueConsts tc tps vcs n)
            _ -> do
                    let sc = generalize env $ foldr1 (.) [TFunc t | t <- vts] (TCon tc tps)
                    let scope (e, d) = (Map.insert vn sc (Map.delete vn e), Map.insert tc (vn : Map.findWithDefault [] tc d) d)
                    local scope (createValueConsts tc tps vcs n)

inferVarDecl :: UntypedDecl -> Infer (TypedDecl, Scheme)
inferVarDecl (DVar tM id e) = do
    env <- askEnv
    ((e', t), c) <- listen $ fixPoint id e
    s <- liftEither $ runSolve c 
    let t1 = apply s t
        sc = generalize env t1 
    constrainIf (tM, t1)
    return (DVar tM id e', sc)
inferVarDecl _ = error "Not possible"

fixPoint :: String -> UntypedExpr -> Infer (TypedExpr, Type)
fixPoint id e = do
    let e1 = EFunc () Nothing Nothing id e
    (e', t1) <- infer e1
    tv <- fresh
    constrain $ (tv `TFunc` tv) :~ t1
    return (e', tv)

inferBlock :: [UntypedDecl] -> [TypedDecl] -> Infer ([TypedDecl], Type)
inferBlock [] _ = throwError EmptyBlock
inferBlock (d : ds) tds =
    case d of
        DStmt (SPass e) -> do
            (e', t) <- infer e
            return (reverse $ DStmt (SPass e') : tds, t)
        DStmt s -> do
            s' <- inferStmt s
            inferBlock ds (DStmt s' : tds)
        DData {} -> throwError BlockData
        DVar _ id _ -> inferVarDecl d >>= \(td', sc) -> scoped id sc (inferBlock ds (td' : tds))

inferStmt :: UntypedStmt -> Infer TypedStmt
inferStmt = \case
    SExpr e -> SExpr . fst <$> infer e
    SPass e -> throwError GlobalPass

lookupEnv :: String -> Infer Type
lookupEnv id = askEnv >>= \e -> case Map.lookup id e of
    Nothing -> throwError $ NotDefined id
    Just t -> instantiate t

infer :: UntypedExpr -> Infer (TypedExpr, Type)
infer = \case
    EBlock _ decls -> do
        (tds, t) <- inferBlock decls []
        return (EBlock t tds, t)
    EAssign _ l r -> do
        (l', lt) <- infer l
        (r', rt) <- infer r
        constrain $ lt :~ rt
        return (EAssign lt l' r', lt)
    EIf _ c a b -> do
        (c', ct) <- infer c
        (a', at) <- infer a
        (b', bt) <- infer b
        constrain $ ct :~ TBool
        constrain $ at :~ bt
        return (EIf at c' a' b', at)
    ECall _ f a -> do
        (f', ft) <- infer f
        (a', at) <- infer a
        rt <- fresh
        constrain $ ft :~ (at `TFunc` rt)
        return (ECall rt f' a', rt)
    EBinary _ op l r -> do
        (l', lt) <- infer l
        (r', rt) <- infer r
        tv <- fresh
        let t1 = lt `TFunc` (rt `TFunc` tv)
            t2 = bOpType lt rt op
        constrain $ t1 :~ t2
        return (EBinary tv op l' r', tv)
    EUnary _ op a -> do
        (a', at) <- infer a
        tv <- fresh
        constrain $ (at `TFunc` tv) :~ uOpType at op
        return (EUnary tv op a', tv)
    EIdent _ id -> lookupEnv id <&> \t -> (EIdent t id, t)
    EString _ s -> return (EString TString s, TString)
    EBool _ b -> return (EBool TBool b, TBool)
    EInt _ n -> return (EInt TInt n, TInt)
    EFloat _ n -> return (EFloat TFloat n, TFloat)
    EUnit _ -> return (EUnit TUnit, TUnit)
    EFunc _ pM rM p e -> do
        tv <- fresh
        (e', t) <- scoped p (Forall Set.empty tv) (infer e)
        constrainIf (pM, tv)
        constrainIf (rM, t)
        let ft = tv `TFunc` t
        return (EFunc ft pM rM p e', ft)
    EMatch _ e bs -> do
        (e', et) <- infer e
        (bs', bts) <- unzip <$> mapM (inferBranch et) bs
        let vcs = map ((\(VC c _) -> c) . fst) bs'
        case bts of
            [] -> throwError EmptyMatch
            (bt : bts') -> (EMatch bt e' bs', bt) <$ sequence_ [constrain (t :~ bt) | t <- bts'] <* addMatch (et, vcs)
    
inferBranch :: Type -> (Pattern, UntypedExpr) -> Infer ((Pattern, TypedExpr), Type)
inferBranch mt (p@(VC c vns), e) = do
    ct <- lookupEnv c
    let (rt, ts) = (\l -> (last l, init l)) . funcTypes . reverseFunc $ ct
    constrain $ rt :~ mt
    (e', bt) <- scopedMany vns (Forall Set.empty `map` ts) (infer e)
    return ((p, e'), bt)
    where
        reverseFunc (a `TFunc` b) = reverseFunc a `TFunc` reverseFunc b
        reverseFunc t = t
        funcTypes (a `TFunc` b) = funcTypes a ++ funcTypes b
        funcTypes t = [t]

