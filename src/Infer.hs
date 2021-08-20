{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Infer (annotate) where

import Debug.Trace

import Control.Monad.Except
import Control.Monad.RWS

import Data.Functor
import Data.Functor.Identity
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Type
import BuiltIn
import Name

data TypeError = Mismatch Type Type | NotDefined QualifiedName | NotDefinedMany [TVar] | UnknownOperator Oper | NotMutable QualifiedName
               | EmptyBlock | EmptyMatch | BlockData | GlobalPass | BlockNamespace | InfiniteType TVar Type
               deriving (Show)

type Infer a = RWST Namespace [Constraint] (TEnv, Map.Map Namespace [Namespace], Int) (Except TypeError) a
type Solve a = ExceptT TypeError Identity a

type Subst = Map.Map TVar Type

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
    tvs (t1 :~: t2) = tvs t1 `Set.union` tvs t2
    apply s (t1 :~: t2) = apply s t1 :~: apply s t2

instance Substitutable a => Substitutable [a] where
    tvs l = foldr (Set.union . tvs) Set.empty l
    apply s = map (apply s)

annotate :: UProgram -> Either TypeError TProgram
annotate (Program decls) =
    case runIdentity $ runExceptT $ runRWST (annotateNamespace decls []) Global (defTEnv, Map.empty, 0) of
        Left err -> Left err
        Right (p, _, cs) -> do
            s <- runSolve cs
            return . Program $ fmap (fmap (apply s)) p

compose :: Subst -> Subst -> Subst
compose a b = Map.map (apply a) b `Map.union` a

unify :: Type -> Type -> Solve Subst
unify a b | a == b = return Map.empty
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify a@(TCon (Qualified _ c1) ts1) b@(TCon (Qualified _ c2) ts2)
    | c1 /= c2 = throwError $ Mismatch a b
    | otherwise = unifyMany ts1 ts2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return Map.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ Mismatch (head t1) (head t2)

bind :: TVar -> Type -> Solve Subst
bind v t
    | v `Set.member` tvs t = throwError $ InfiniteType v t
    | otherwise = return $ Map.singleton v t 

solve :: Subst -> [Constraint] -> Solve Subst
solve s c =
    case c of
        [] -> return s
        ((t1 :~: t2) : cs) -> do
            s1 <- unify t1 t2
            let nsub = s1 `compose` s
            solve (s1 `compose` s) (apply s1 cs)

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solve Map.empty cs

fresh :: Infer Type
fresh = do
    (env, m, n) <- get
    put (env, m, n+1)
    return . TVar . flip TV Star $ varNames !! n
    where varNames = map ('_':) $ [1..] >>= flip replicateM ['a'..'z']

generalize :: TEnv -> Type -> Scheme
generalize env t = Forall vs t
    where vs = tvs t `Set.difference` tvs (map fst (Map.elems env))

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
    let vs' = Set.toList vs
    nvs <- mapM (const fresh) vs'
    let s = Map.fromList (zip vs' nvs)
    return $ apply s t

scoped :: QualifiedName -> (Scheme, Bool) -> Infer a -> Infer a
scoped id d toRun = do
    (env, m, n) <- get
    put (Map.insert id d (Map.delete id env), m, n)
    res <- toRun
    (_, m', n') <- get
    put (env, m', n')
    return res

scopedMany :: [(QualifiedName, (Scheme, Bool))] -> Infer a -> Infer a
scopedMany [] m = m
scopedMany ((id, d) : vs) m = scoped id d (scopedMany vs m)

constrain :: Constraint -> Infer ()
constrain = tell . (:[])

valueConstructors :: QualifiedName -> [TVar] -> [(QualifiedName, [Type])] -> Infer ()
valueConstructors _ _ [] = return ()
valueConstructors tc tps ((vn, vts) : vcs) = do
    (env, m, n) <- get
    let tps' = map TVar tps
    let vtps = tvs tps'
    let vvts = tvs vts
    if (vtps `Set.intersection` vvts) /= vvts
        then throwError $ NotDefinedMany (Set.toList (vvts `Set.difference` vtps))
        else let sc = generalize env $ foldr (:->) (TCon tc tps') vts
             in put (Map.insert vn (sc, False) (Map.delete vn env), m, n) *> valueConstructors tc tps vcs

annotateNamespace :: [UDecl] -> [TDecl] -> Infer [TDecl]
annotateNamespace [] tds = return $ reverse tds
annotateNamespace (d : ds) tds =
    case d of
        DStmt s -> annotateStmt s >>= \s' -> annotateNamespace ds (DStmt s' : tds)
        DData tc tps vcs -> valueConstructors tc tps vcs *> annotateNamespace ds (DData tc tps vcs: tds)
        DVar m _ id _ -> inferVarDecl d >>= \(td', sc) -> do
            (env, impMap, n) <- get
            put (Map.insert id (sc, m) (Map.delete id env), impMap, n)
            annotateNamespace ds (td' : tds)
        DNamespace name nds imps -> do
            ns <- ask
            (e, m, n) <- get
            let newns = Relative ns name
            put (e, Map.insert newns imps m, n)
            nds' <- local (const newns) (annotateNamespace nds [])
            annotateNamespace ds (DNamespace name nds' imps : tds)

annotateStmt :: UStmt -> Infer TStmt
annotateStmt = \case
    SExpr e -> infer e <&> SExpr . fst
    SPass _ -> throwError GlobalPass

inferVarDecl :: UDecl -> Infer (TDecl, Scheme)
inferVarDecl (DVar m ta id e) = do
    (env, _, _) <- get
    recurType <- fresh
    ((e', t), c) <- listen $ scoped id (Forall Set.empty recurType, False) (infer e)
    s <- liftEither $ runSolve c
    let t' = apply s t
        sc = generalize env t'
    when (isJust ta) (constrain $ fromJust ta :~: t')
    constrain $ recurType :~: t'
    return (DVar m ta id e', sc)
inferVarDecl _ = error "Not possible"

inferBlock :: [UDecl] -> [TDecl] -> Infer ([TDecl], Type)
inferBlock [] _ = throwError EmptyBlock
inferBlock (d : ds) tds =
    case d of
        DStmt (SPass e) -> do
            (e', t) <- infer e
            return (reverse $ DStmt (SPass e') : tds, t)
        DStmt s -> do
            s' <- annotateStmt s
            inferBlock ds (DStmt s' : tds)
        DData {} -> throwError BlockData
        DVar m _ id _ -> inferVarDecl d >>= \(td', sc) -> scoped id (sc, m) (inferBlock ds (td' : tds))
        DNamespace {} -> throwError BlockNamespace

inferLit :: Lit -> Infer (Lit, Type)
inferLit = \case
    LInt n -> return (LInt n, TInt)
    LFloat n -> return (LFloat n, TFloat)
    LBool b -> return (LBool b, TBool)
    LChar c -> return (LChar c, TChar)
    LUnit -> return (LUnit, TUnit)

infer :: UExpr -> Infer (TExpr, Type)
infer = \case
    EIdent _ id -> lookupType id >>= \t -> return (EIdent t id, t)
    ELit _ l -> inferLit l >>= \(l', t) -> return (ELit t l', t)
    EFunc _ p e -> do
        pt <- fresh
        ns <- ask -- SHOULD BE RESOLVED ALREADY
        (e', rt) <- scoped (Qualified ns p) (Forall Set.empty pt, False) (infer e)
        let t = pt :-> rt
        return (EFunc t p e', t)
    EIf _ c a b -> do
        (c', ct) <- infer c
        (a', at) <- infer a
        (b', bt) <- infer b
        constrain $ ct :~: TBool
        constrain $ at :~: bt
        return (EIf at c' a' b', at)
    EMatch _ e bs -> do
        (e', et) <- infer e
        (bs', bts) <- unzip <$> mapM (inferBranch et) bs
        case bts of
            [] -> throwError EmptyMatch
            (bt : bts') -> (EMatch bt e' bs', bt) <$ sequence_ [constrain $ bt' :~: bt | bt' <- bts']
    EBlock _ ds -> inferBlock ds [] >>= \(ds', t) -> return (EBlock t ds', t)
    EBinary _ op l r -> do
        (l', lt) <- infer l
        (r', rt) <- infer r
        t <- fresh
        let t1 = lt :-> (rt :-> t)
        ns <- ask -- SHOULD BE RESOLVED ALREADY
        let op' = Qualified ns op
        t2 <- lookupType op'
        constrain $ t1 :~: t2
        return (ECall t (ECall (rt :-> t) (EIdent t2 op') l') r', t)--(EBinary t op l' r', t)
    EUnary _ op a -> do
        (a', at) <- infer a
        t <- fresh
        ns <- ask -- SHOULD BE RESOLVED ALREADY
        let op' = Qualified ns op
        ot <- lookupType op'
        constrain $ (at :-> t) :~: ot
        return (ECall t (EIdent ot op') a', t) -- (EUnary t op a', t)
    EAssign _ id r -> do
        (r', rt) <- infer r
        idt <- lookupType id
        mut <- lookupMut id
        if mut
            then constrain (idt :~: rt) >> return (EAssign idt id r', idt)
            else throwError $ NotMutable id
    ECall _ f a -> do
        (f', ft) <- infer f
        (a', at) <- infer a
        rt <- fresh
        constrain $ ft :~: (at :-> rt)
        return (ECall rt f' a', rt)

inferPattern :: Pattern -> Infer (Type, [(String, Scheme)])
inferPattern (PLit l) = inferLit l >>= \(_, t) -> return (t, [])
inferPattern (PVar id) = fresh <&> \t -> (t, [(id, Forall Set.empty t)])
inferPattern (PCon con ps) = do
    (pts, vars) <- unzip <$> mapM inferPattern ps
    ft <- lookupType con
    t <- fresh
    let ft' = foldr (:->) t pts
    constrain $ ft' :~: ft
    return (t, concat vars)

inferBranch :: Type -> (Pattern, UExpr) -> Infer ((Pattern, TExpr), Type)
inferBranch mt (p, e) = do
    (pt, vars) <- inferPattern p
    constrain $ pt :~: mt
    ns <- ask -- SHOULD BE RESOLVED ALREADY
    (e', et) <- scopedMany (map (\(id, sc) -> (Qualified ns id, (sc, False))) vars) (infer e)
    return ((p, e'), et)

------------
-- Lookup --
------------

lookupType :: QualifiedName -> Infer Type
lookupType name = lookupEnv name >>= instantiate . fst

lookupMut :: QualifiedName -> Infer Bool
lookupMut name = snd <$> lookupEnv name

lookupEnv :: QualifiedName -> Infer (Scheme, Bool)
lookupEnv name = do
    res <- lookupEnv' name name
    case res of
        Nothing -> throwError $ NotDefined name
        Just res' -> return res'

lookupEnv' :: QualifiedName -> QualifiedName -> Infer (Maybe (Scheme, Bool))
lookupEnv' name orig = do
    (env, m, _) <- get
    case Map.lookup name env of
        Just x -> return (Just x)
        Nothing -> case name of
            (Qualified Global _) -> do
                ns <- ask
                let imps = fromMaybe [] (Map.lookup ns m)
                lookupImports imps orig
            (Qualified (Relative parent _) s) -> lookupEnv' (Qualified parent s) orig

lookupImports :: [Namespace] -> QualifiedName -> Infer (Maybe (Scheme, Bool))
lookupImports [] _ = return Nothing
lookupImports (i : is) name@(Qualified ns os) = do
    let name' = Qualified i os
    res <- local (const i) (lookupEnv' name' name')
    case res of
        Just _ -> return res
        Nothing -> lookupImports is name

