{-# Language LambdaCase #-}

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

data TypeError = Mismatch Type Type | NotDefined Ident | NotDefinedMany [TVar] | UnknownOperator Oper | NotMutable Ident
               | EmptyBlock | EmptyMatch | BlockData | GlobalPass | InfiniteType TVar Type
               deriving (Show)

type TEnv = Map.Map Ident (Scheme, Bool)
type Infer a = RWST TEnv [Constraint] Int (Except TypeError) a
type Solve a = ExceptT TypeError Identity a

type Subst = Map.Map TVar Type

defTEnv :: TEnv
defTEnv = Map.fromList
    [("addInt", (Forall Set.empty (TInt :-> (TInt :-> TInt)), False)),
     ("subInt", (Forall Set.empty (TInt :-> (TInt :-> TInt)), False)),
     ("mulInt", (Forall Set.empty (TInt :-> (TInt :-> TInt)), False)),
     ("divInt", (Forall Set.empty (TInt :-> (TInt :-> TInt)), False)),
     ("expInt", (Forall Set.empty (TInt :-> (TInt :-> TInt)), False)),
     ("addFloat", (Forall Set.empty (TFloat :-> (TFloat :-> TFloat)), False)),
     ("error", (Forall (Set.fromList [TV "a" Star]) (TCon "List" [TChar] :-> TVar (TV "a" Star)), False)),
     ("bottom", (Forall (Set.fromList [TV "a" Star]) (TVar (TV "a" Star)), False))]

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
    case runIdentity $ runExceptT $ runRWST (annotateProgram decls []) defTEnv 0 of
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
unify a@(TCon c1 ts1) b@(TCon c2 ts2)
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
    n <- get
    put (n+1)
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

scoped :: Ident -> (Scheme, Bool) -> Infer a -> Infer a
scoped id d = local (Map.insert id d . Map.delete id)

scopedMany :: [(Ident, (Scheme, Bool))] -> Infer a -> Infer a
scopedMany [] m = m
scopedMany ((id, d) : vs) m = scoped id d (scopedMany vs m)

lookupType :: Ident -> Infer Type
lookupType id = lookupEnv id >>= instantiate . fst

lookupMut :: Ident -> Infer Bool
lookupMut id = snd <$> lookupEnv id

lookupEnv :: Ident -> Infer (Scheme, Bool)
lookupEnv id = ask >>= \e ->
    case Map.lookup id e of
        Nothing -> throwError $ NotDefined id
        Just d -> return d

constrain :: Constraint -> Infer ()
constrain = tell . (:[])

valueConstructors :: String -> [TVar] -> [(String, [Type])] -> Infer a -> Infer a
valueConstructors _ _ [] n = n
valueConstructors tc tps ((vn, vts) : vcs) n = do
    env <- ask
    let tps' = map TVar tps
    let vtps = tvs tps'
    let vvts = tvs vts
    if (vtps `Set.intersection` vvts) /= vvts
        then throwError $ NotDefinedMany (Set.toList (vvts `Set.difference` vtps))
        else let sc = generalize env $ foldr (:->) (TCon tc tps') vts
             in local (Map.insert vn (sc, False) . Map.delete vn) (valueConstructors tc tps vcs n)

annotateProgram :: [UDecl] -> [TDecl] -> Infer [TDecl]
annotateProgram [] tds = return $ reverse tds
annotateProgram (d : ds) tds =
    case d of
        DStmt s -> annotateStmt s >>= \s' -> annotateProgram ds (DStmt s' : tds)
        DData tc tps vcs -> valueConstructors tc tps vcs (annotateProgram ds (DData tc tps vcs: tds))
        DVar m _ id _ -> inferVarDecl d >>= \(td', sc) -> scoped id (sc, m) (annotateProgram ds (td' : tds))

annotateStmt :: UStmt -> Infer TStmt
annotateStmt = \case
    SExpr e -> infer e <&> SExpr . fst
    SPass _ -> throwError GlobalPass

inferVarDecl :: UDecl -> Infer (TDecl, Scheme)
inferVarDecl (DVar m ta id e) = do
    env <- ask
    ((e', t), c) <- listen $ fixPoint id e
    s <- liftEither $ runSolve c
    let t' = apply s t
        sc = generalize env t'
    when (isJust ta) (constrain $ fromJust ta :~: t')
    return (DVar m ta id e', sc)
inferVarDecl _ = error "Not possible"

fixPoint :: String -> UExpr -> Infer (TExpr, Type)
fixPoint id e = do
    let e1 = EFunc () id e
    (e1', t1) <- infer e1
    case e1' of
        (EFunc _ _ e') -> do
            tv <- fresh
            constrain $ (tv :-> tv) :~: t1
            return (e', tv)
        _ -> error "Not possible"

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

infer :: UExpr -> Infer (TExpr, Type)
infer = \case
    EIdent _ id -> lookupType id >>= \t -> return (EIdent t id, t)
    EInt _ n -> return (EInt TInt n, TInt)
    EFloat _ n -> return (EFloat TFloat n, TFloat)
    EBool _ b -> return (EBool TBool b, TBool)
    EChar _ c -> return (EChar TChar c, TChar)
    EUnit _ -> return (EUnit TUnit, TUnit)
    EFunc _ p e -> do
        pt <- fresh
        (e', rt) <- scoped p (Forall Set.empty pt, False) (infer e)
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
        t2 <- lookupType op
        {-
        t2 <- case op of
            _ | op `elem` ["+", "-", "*", "/", "^"] -> return $ TInt :-> (TInt :-> TInt)
            _ | op `elem` [">", "<", ">=", "<="] -> return $ TInt :-> (TInt :-> TBool)
            _ | op `elem` ["==", "!="] -> return $ lt :-> (rt :-> TBool)
            _ | op `elem` ["||", "&&"] -> return $ TBool :-> (TBool :-> TBool)
            _ -> throwError $ UnknownOperator op
        -}
        constrain $ t1 :~: t2
        return (ECall t (ECall (rt :-> t) (EIdent t2 op) l') r', t)--(EBinary t op l' r', t)
    EUnary _ op a -> do
        (a', at) <- infer a
        t <- fresh
        ot <- lookupType op
        {-
        ot <- case op of
            "-" -> return $ TInt :-> TInt
            "!" -> return $ TBool :-> TBool
            _ -> throwError $ UnknownOperator op
        -}
        constrain $ (at :-> t) :~: ot
        return (ECall t (EIdent ot op) a', t) -- (EUnary t op a', t)
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

inferPattern :: Pattern -> Infer (Type, [(Ident, Scheme)])
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
    (e', et) <- scopedMany (map (\(id, sc) -> (id, (sc, False))) vars) (infer e)
    return ((p, e'), et)
