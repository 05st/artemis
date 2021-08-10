{-# Language LambdaCase #-}
{-# Language TypeSynonymInstances #-}

module Infer (typecheck) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST
import Type
import Kind

data TypeError = Mismatch Type Type | NotFunction Type | NotDefined String | Redefinition String
               | EmptyBlock | GlobalPass | BlockData
               | UnifyError [Type] [Type] | InfiniteType TVar Type deriving (Show)

type Infer a = RWST Env [Constraint] Int (Except TypeError) a
type Solver a = ExceptT TypeError Identity a
type Env = Map.Map String Scheme
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
    tvs (t1 :~ t2) = tvs t1 `Set.union` tvs t2
    apply s (t1 :~ t2) = apply s t1 :~ apply s t2

instance Substitutable a => Substitutable [a] where
    tvs l = foldr (Set.union . tvs) Set.empty l
    apply s = map (apply s)

fresh :: Infer Type
fresh = do
    n <- get
    put $ n+1
    return . TVar . TV $ (varNames !! n)
    where varNames = map ('$':) $ [1..] >>= flip replicateM ['a'..'z']

constrain :: Constraint -> Infer ()
constrain = tell . (:[])

compose :: Subst -> Subst -> Subst
compose a b = Map.map (apply a) b `Map.union` a

generalize :: Env -> Type -> Scheme
generalize env t = Forall vs t
    where vs = tvs t `Set.difference` tvs (Map.elems env)

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
    let vs' = Set.toList vs
    nvs <- mapM (const fresh) vs'
    return $ apply (Map.fromList (zip vs' nvs)) t

unify :: Type -> Type -> Solver Subst
unify a b | a == b = return Map.empty
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify a@(TCon c1 ts1) b@(TCon c2 ts2)
    | a /= b = throwError $ Mismatch a b
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

typecheck :: Program -> Either TypeError String
typecheck decls = case runIdentity $ runExceptT $ runRWST (inferProgram decls) Map.empty 0 of
    Left err -> Left err
    Right (_, _, cs) -> Right $ show (runSolve cs) ++ show cs

scoped :: String -> Scheme -> Infer a -> Infer a
scoped x sc m = do
    let scope e = Map.insert x sc (Map.delete x e)
    local scope m

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

inferProgram :: [Decl] -> Infer ()
inferProgram [] = return ()
inferProgram (d : ds) =
    case d of
        DStmt s -> inferStmt s *> inferProgram ds
        DData {} -> inferProgram ds -- TODO
        DVar {} -> inferVarDecl d (inferProgram ds)

inferVarDecl :: Decl -> Infer a -> Infer a
inferVarDecl (DVar tM id e) next = do
    env <- ask
    (t, c) <- listen $ fixPoint id e
    s <- liftEither $ runSolve c
    let t1 = apply s t
        sc = generalize env t1
    scoped id sc next
inferVarDecl _ _ = undefined -- Not possible

fixPoint :: String -> Expr -> Infer Type
fixPoint id e = do
    let e1 = EFunc Nothing Nothing id e
    t1 <- infer e1
    tv <- fresh
    constrain $ (tv `TFunc` tv) :~ t1
    return tv

inferBlock :: [Decl] -> Infer Type
inferBlock [] = throwError EmptyBlock
inferBlock (d : ds) =
    case d of
        DStmt (SPass e) -> infer e
        DStmt s -> inferStmt s *> inferBlock ds
        DData {} -> throwError BlockData
        DVar {} -> inferVarDecl d (inferBlock ds)

inferStmt :: Stmt -> Infer ()
inferStmt = \case
    SExpr e -> void $ infer e
    SPass e -> throwError GlobalPass

infer :: Expr -> Infer Type
infer = \case
    EBlock decls -> undefined
    EAssign l r -> do
        lt <- infer l
        rt <- infer r
        constrain $ lt :~ rt
        return lt
    EIf c a b -> do
        ct <- infer c
        at <- infer a
        bt <- infer b
        constrain $ ct :~ TBool
        constrain $ at :~ bt
        return at
    ECall f a -> do
        ft <- infer f
        at <- infer a
        tv <- fresh
        constrain $ ft :~ (at `TFunc` tv)
        return tv
    EBinary op l r -> do
        lt <- infer l
        rt <- infer r
        tv <- fresh
        let t1 = lt `TFunc` (rt `TFunc` tv)
            t2 = bOpType lt rt op
        constrain $ t1 :~ t2
        return tv
    EUnary op a -> do
        at <- infer a
        tv <- fresh
        constrain $ (at `TFunc` tv) :~ uOpType at op
        return tv
    EIdent id -> do
        env <- ask
        case Map.lookup id env of
            Nothing -> throwError $ NotDefined id
            Just t -> instantiate t
    EString _ -> return TString
    EBool _ -> return TBool
    EInt _ -> return TInt
    EFloat _ -> return TFloat
    EUnit -> return TUnit
    EFunc pM rM p e -> do
        tv <- fresh
        t <- scoped p (Forall Set.empty tv) (infer e)
        return $ tv `TFunc` t



{-
primTEnv :: TEnv
primTEnv = Map.empty

extend :: String -> Scheme -> TEnv -> TEnv
extend = Map.insert

varNames :: [String]
varNames = Prelude.map ('$':) $ [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    (s, n) <- get
    put (s, n + 1)
    return $ TVar (varNames !! n)

freshMaybe :: Maybe Type -> Infer Type
freshMaybe Nothing = fresh
freshMaybe (Just t) = return t --addTVarsRecursive t >> return t

addTVarsRecursive :: Type -> Infer ()
addTVarsRecursive (TCon _ []) = return ()
addTVarsRecursive (TCon _ ps) = mapM_ addTVarsRecursive ps
addTVarsRecursive t@(TVar _) = do
    (s, n) <- get
    put (Map.insert t t s, n)

addConst :: Type -> Type -> Infer ()
addConst a b = tell [CEq a b]

typecheck :: [Stmt] -> Either TypeError String
typecheck stmts = case runExcept (runRWST (checkProgram stmts) primTEnv (Map.empty, 0)) of
    Left err -> Left err
    Right (_, (s, _), c) -> Right $ show (runSolve c s) ++ show (length c)

scoped :: String -> Scheme -> Infer a -> Infer a
scoped x sc m = do
    let scope e = extend x sc (Map.delete x e)
    local scope m

subEnv :: Subst -> TEnv -> TEnv
subEnv s = Map.map (subScheme s)
    where subScheme s (Forall as t) = Forall as $ substitute (Prelude.foldr Map.delete s as) t

checkProgram :: [Stmt] -> Infer ()
checkProgram ((SExpr e) : stmts) = inferExpr e >> checkProgram stmts
checkProgram ((SVar tM id e) : stmts) = do
    env <- ask
    case Map.lookup id env of
        Just _ -> throwError $ AlreadyDefined id
        Nothing -> do
            (t, c) <- listen $ fixPoint id e
            (s, _) <- get
            subst <- liftEither $ runSolve c s
            let t1 = substitute subst t
                sc = generalize env t1
            scoped id sc (checkProgram stmts)
checkProgram ((SPass e) : stmts) = throwError PassOutOfBlock
checkProgram ((SData tcon tcvars vcons) : stmts) = checkProgram stmts -- TODO
checkProgram [] = return ()

fixPoint :: String -> Expr -> Infer Type
fixPoint id e = do
    let e1 = EFunc Nothing Nothing id e
    t1 <- inferExpr e1
    tv <- fresh
    addConst (TFunc tv tv) t1
    return tv

inferBlock :: [Stmt] -> Infer Type
inferBlock ((SExpr e) : stmts) = inferExpr e >> inferBlock stmts
inferBlock ((SVar tM id e) : stmts) = do
    env <- ask
    (t, c) <- listen $ inferExpr e
    (s, _) <- get
    subst <- liftEither $ runSolve c s
    let t1 = substitute subst t
        sc = generalize env t1
    tv <- freshMaybe tM
    addConst t1 tv
    scoped id sc (inferBlock stmts)
inferBlock ((SPass e) : stmts) = inferExpr e
inferBlock (SData {} : stmts) = throwError DataDeclInBlock
inferBlock [] = throwError EmptyBlock

inferExpr :: Expr -> Infer Type
inferExpr = \case
    EBlock stmts -> inferBlock stmts
    EAssign l r -> do
        lt <- inferExpr l
        rt <- inferExpr r
        addConst lt rt
        return lt
    EIf c a b -> do
        ct <- inferExpr c
        addConst TBool ct
        at <- inferExpr a
        bt <- inferExpr b
        addConst at bt
        return at
    ECall f a -> do
        t1 <- inferExpr f
        t2 <- inferExpr a
        tv <- fresh
        addConst t1 (TFunc t2 tv)
        return tv
    EBinary op l r -> do
        lt <- inferExpr l
        rt <- inferExpr r
        case op of
            _ | op `elem` [Add, Sub, Mul, Div] -> addConst TInt lt >> addConst TInt rt >> return TInt
            _ | op `elem` [Greater, GreaterEqual, Lesser, LesserEqual] -> addConst TInt lt >> addConst TInt rt >> return TBool
            _ | op `elem` [Or, And] -> addConst TBool lt >> addConst TBool rt >> return TBool
            _ | op `elem` [Equal, NotEqual] -> addConst lt rt >> return TBool
            _ -> throwError Unknown
    EUnary op x -> do
        xt <- inferExpr x
        case op of
            Sub -> addConst TInt xt >> return TInt
            Not -> addConst TBool xt >> return TBool
            _ -> throwError Unknown
    EIdent s -> do
        env <- ask
        case Map.lookup s env of
            Nothing -> throwError $ NotDefined s
            Just (Forall as t) -> do
                ptvs <- mapM (const fresh) as
                let inst = zip as ptvs
                let vt = instantiate inst t
                return vt
    EString _ -> return TString
    EBool _ -> return TBool
    EInt _ -> return TInt
    EFloat _ -> return TFloat
    EFunc pM rM p e -> do
        pt <- freshMaybe pM
        rt <- freshMaybe rM
        et <- scoped p (Forall [] pt) (inferExpr e)
        addConst rt et
        return (TFunc pt rt)
    EUnit -> return TUnit


instantiate :: [(Type, Type)] -> Type -> Type
instantiate [] t = t
instantiate inst t = substitute (Map.fromList inst) t

generalize :: TEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ tvs t `Set.difference` tvsEnv env
          tvsEnv = tvsList . Map.elems
          tvsList = Prelude.foldr (Set.union . tvsScheme) Set.empty 
          tvsScheme (Forall as t) = tvs t `Set.difference` Set.fromList as
    
substitute :: Subst -> Type -> Type
substitute s = \case
    TCon ss ps -> TCon ss (Prelude.map (substitute s) ps)
    a@(TVar _) -> fromMaybe a (Map.lookup a s)

tvs :: Type -> Set Type
tvs (TCon _ ps) = Prelude.foldr (Set.union . tvs) Set.empty ps
tvs a@(TVar _) = Set.singleton a

occurs :: Type -> Type -> Bool
occurs a b = a `Set.member` tvs b

unify :: Type -> Type -> ExceptT TypeError (State Subst) ()
unify a b | a == b = return ()
unify a@(TVar _) b = bind a b 
unify a b@(TVar _) = bind b a
unify (TFunc fa fb) (TFunc ga gb) = unify fa ga >> get >>= \s -> unify (substitute s fb) (substitute s gb)
unify a b = throwError $ Mismatch a b

bind :: Type -> Type -> ExceptT TypeError (State Subst) ()
bind a t | t == a = return ()
         | occurs a t = throwError $ InfiniteType a t
         | otherwise = do
            s <- get
            case Map.lookup a s of
                Nothing -> put (Map.insert a t s) -- TODO: better error message
                Just a' -> if a /= a'
                    then throwError $ Mismatch a' t
                    else put (Map.insert a t s)

solve :: [Constraint] -> ExceptT TypeError (State Subst) Subst
solve ((CEq a b) : cs) = do
    s <- get
    unify (substitute s a) (substitute s b)
    s' <- get
    put (Map.map (substitute s') s')
    solve cs
solve [] = get

runSolve :: [Constraint] -> Subst -> Either TypeError Subst
runSolve c s = case runState (runExceptT (solve c)) s of
    (Left err, _) -> Left err
    (Right _, s) -> Right s
-}
