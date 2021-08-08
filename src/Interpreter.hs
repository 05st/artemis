{-# Language LambdaCase #-}

module Interpreter (typecheck) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map as Map
import Data.Set as Set

import AST
import Type

data TypeError = Mismatch Type Type | NotFunction Type | NotDefined String
               | EmptyBlock | PassOutOfBlock | DataDeclInBlock
               | UnifyError Type Type | InfiniteType Type Type | Unknown deriving (Show)

type Infer a = RWST TEnv [Constraint] (Subst, Int) (Except TypeError) a
type TEnv = Map String Scheme
type Subst = Map Type Type

primTEnv :: TEnv
primTEnv = Map.empty

extend :: String -> Scheme -> TEnv -> TEnv
extend = Map.insert

varNames :: [String]
varNames = Prelude.map ('$':) $ [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    (s, n) <- get
    let t = TVar (varNames !! n)
    put (Map.insert t t s, n + 1)
    return t

freshMaybe :: Maybe Type -> Infer Type
freshMaybe Nothing = fresh
freshMaybe (Just t) = addTVarsRecursive t >> return t

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
    Right (_, (s, _), c) -> Right . show $ runSolve c s

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
    (t, c) <- listen $ fixPoint id e
    (s, _) <- get
    subst <- liftEither $ runSolve c s
    let t1 = substitute subst t
        sc = generalize env t1
    tv <- freshMaybe tM
    addConst t1 tv
    scoped id sc (checkProgram stmts)
checkProgram ((SPass e) : stmts) = throwError PassOutOfBlock
checkProgram ((SData tcon tcvars vcons) : stmts) = checkProgram stmts -- TODO
checkProgram [] = return ()

fixPoint :: String -> Expr -> Infer Type
fixPoint id e = do
    let e1 = EItem $ IFunc Nothing Nothing id e
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
    EItem i -> inferItem i
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
    ECall a f -> do
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

instantiate :: [(Type, Type)] -> Type -> Type
instantiate [] t = t
instantiate inst t = substitute (Map.fromList inst) t

generalize :: TEnv -> Type -> Scheme
generalize env t = Forall as t
    where as = Set.toList $ tvs t `Set.difference` tvsEnv env
          tvsEnv = tvsList . Map.elems
          tvsList = Prelude.foldr (Set.union . tvsScheme) Set.empty 
          tvsScheme (Forall as t) = tvs t `Set.difference` Set.fromList as

inferItem :: Item -> Infer Type
inferItem = \case
    IIdent s -> do
        env <- ask
        case Map.lookup s env of
            Nothing -> throwError $ NotDefined s
            Just (Forall as t) -> do
                ptvs <- mapM (const fresh) as
                let inst = zip as ptvs
                let vt = instantiate inst t
                return vt
    IString _ -> return TString
    IBool _ -> return TBool
    IInt _ -> return TInt
    IFloat _ -> return TFloat
    IFunc pM rM p e -> do
        pt <- freshMaybe pM
        rt <- freshMaybe rM
        et <- scoped p (Forall [] pt) (inferExpr e)
        addConst rt et
        return (TFunc pt rt)
    IUnit -> return TUnit

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
                Nothing -> throwError $ UnifyError a t -- TODO: better error message
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
