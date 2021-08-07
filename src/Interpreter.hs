{-# Language LambdaCase #-}

module Interpreter (typecheck) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map
import Data.Set

import AST
import Type

data TypeError = Mismatch Type Type | MismatchMult [Type] Type | NotFunction Type | NotDefined String | EmptyBlock | PassOutOfBlock | UnifyError Type Type | InfiniteType Type Type | Unknown deriving (Show)

type Infer a = RWST TEnv [Constraint] (Subst, Int) (Except TypeError) a
type TEnv = [(String, Type)]

type Subst = Map Int Type

extend :: String -> Type -> TEnv -> TEnv
extend s t e = (s, t):e

fresh :: Infer Type
fresh = do
    (s, n) <- get
    let t = TVar (n + 1)
    put (Data.Map.insert (n + 1) t s, n + 1)
    return t

freshMaybe :: Maybe Type -> Infer Type
freshMaybe Nothing = fresh
freshMaybe (Just t) = return t

addConst :: Type -> Type -> Infer ()
addConst a b = tell [CEquality a b]

typecheck :: [Stmt] -> Either TypeError String
typecheck stmts = case runExcept (runRWST (checkProgram stmts) [] (Data.Map.empty, 0)) of
    Left err -> Left err
    Right (_, (s, _), c) -> Right $ '\n':show (runSolve c s) ++ " | \n\n" ++ show c ++ " | \n\n" ++ show s

checkProgram :: [Stmt] -> Infer ()
checkProgram ((SExpr e) : stmts) = inferExpr e >> checkProgram stmts
checkProgram ((SVar tM id e) : stmts) = do
    t <- freshMaybe tM
    et <- local (extend id t) (inferExpr e)
    addConst t et
    local (extend id t) (checkProgram stmts)
checkProgram ((SPass e) : stmts) = throwError PassOutOfBlock
checkProgram [] = return ()

inferBlock :: [Stmt] -> Infer Type
inferBlock ((SExpr e) : stmts) = inferExpr e >> inferBlock stmts
inferBlock ((SVar tM id e) : stmts) = do
    t <- freshMaybe tM
    et <- local (extend id t) (inferExpr e)
    addConst t et
    local (extend id t) (inferBlock stmts)
inferBlock ((SPass e) : stmts) = inferExpr e
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
        addConst tBool ct
        at <- inferExpr a
        bt <- inferExpr b
        addConst at bt
        return at
    ECall a f -> do
        ft <- inferExpr f
        at <- inferExpr a
        rt <- fresh
        addConst ft (TFunc at rt) 
        return rt
    EBinary op l r -> do
        lt <- inferExpr l
        rt <- inferExpr r
        case op of
            _ | op `elem` [Add, Sub, Mul, Div] -> addConst tInt lt >> addConst tInt rt >> return tInt
            _ | op `elem` [Greater, GreaterEqual, Lesser, LesserEqual] -> addConst tInt lt >> addConst tInt rt >> return tBool
            _ | op `elem` [Or, And] -> addConst tBool lt >> addConst tBool rt >> return tBool
            _ | op `elem` [Equal, NotEqual] -> addConst lt rt >> return tBool
            _ -> throwError Unknown
    EUnary op x -> do
        xt <- inferExpr x
        case op of
            Sub -> addConst tInt xt >> return tInt
            Not -> addConst tBool xt >> return tBool
            _ -> throwError Unknown

inferItem :: Item -> Infer Type
inferItem = \case
    IIdent s -> do
        env <- ask
        case Prelude.lookup s env of
            Nothing -> throwError $ NotDefined s
            Just t -> return t
    IString _ -> return tString
    IBool _ -> return tBool
    IInt _ -> return tInt
    IFloat _ -> return tFloat
    IFunc pM rM p e -> do
        pt <- freshMaybe pM
        rt <- freshMaybe rM
        et <- local (extend p pt) (inferExpr e)
        addConst rt et
        return (TFunc pt rt)
    IUnit -> return tUnit

substitute :: Type -> ExceptT TypeError (State Subst) Type
substitute = \case
    a@TCon {} -> return a
    a@(TVar i) -> get >>= \s -> return $ fromMaybe a (Data.Map.lookup i s)
    (TFunc a b) -> do
        p <- substitute a
        r <- substitute b
        return $ TFunc p r

tvs :: Type -> Set Type
tvs (TCon _) = Data.Set.empty
tvs a@(TVar _) = Data.Set.singleton a
tvs (TFunc a b) = tvs a `Data.Set.union` tvs b

occurs :: Type -> Type -> Bool
occurs a b = a `Data.Set.member` tvs b

unify :: Type -> Type -> ExceptT TypeError (State Subst) ()
unify a b | a == b = return ()
unify a@(TVar i) b = get >>= put . Data.Map.insert i b
unify a b@(TVar i) = get >>= put . Data.Map.insert i a
unify (TFunc fa fb) (TFunc ga gb) = unify fa ga >> unify fb gb
unify a b = throwError $ Mismatch a b

solve :: [Constraint] -> ExceptT TypeError (State Subst) Subst
solve ((CEquality a b) : cs) = do
    a' <- substitute a
    b' <- substitute b
    unify a' b'
    solve cs
solve [] = get

runSolve :: [Constraint] -> Subst -> Either TypeError Subst
runSolve c s = case runState (runExceptT (solve c)) s of
    (Left err, _) -> Left err
    (Right _, s) -> Right s

{-

sub :: Subst -> Type -> Type
sub s = \case
    TCon a -> TCon a
    t@(TVar i) -> Data.Map.findWithDefault t i s
    (TFunc a b) -> TFunc (sub s a) (sub s b)

unify :: Type -> Type -> ExceptT TypeError (State Subst) Subst
unify (TFunc a b) (TFunc a' b') = do
    s1 <- unify a a'
    s2 <- unify (sub s1 b) (sub s1 b')
    return (Data.Map.map (sub s1) s2 `Data.Map.union` s1)
unify a@(TVar _) b = bind a b
unify a b@(TVar _) = bind b a
unify (TCon a) (TCon b) | a == b = return Data.Map.empty
unify a b = throwError $ UnifyError a b

bind :: Type -> Type -> ExceptT TypeError (State Subst) Subst
bind a@(TVar i) b
    | a == b = return Data.Map.empty
    | occurs a b = throwError $ InfiniteType a b
    | otherwise = return $ Data.Map.singleton i b
bind a b = throwError $ UnifyError a b


solve :: [Constraint] -> Subst -> Either TypeError Subst
solve stmts subst = case runState (runExceptT (go stmts)) subst of
        (Left err, _) -> Left err
        (Right _, s) -> Right s
    where
        go ((CEquality a b) : cs) = go cs >>= Data.Map.union (unify a b)
        go [] = get
subAll :: Type -> Type -> Type -> Type
subAll a b t | t == a = b
subAll a b (TFunc fa fb) = TFunc (subAll a b fa) (subAll a b fb)
subAll a b t = t

unify :: Type -> Type -> ExceptT TypeError (State Subst) ()
unify a b | a == b = return ()
unify at@(TVar i) b = do
    s <- get
    case Data.Map.lookup i s of
        Nothing -> throwError $ NotDefined ('$':show i)
        Just t -> if t /= at
            then unify t b 
            else put $ Data.Map.map (subAll t b) $ insert i b s
unify a bt@(TVar i) = do
    s <- get
    case Data.Map.lookup i s of
        Nothing -> throwError $ NotDefined ('$':show i)
        Just t -> if t /= bt
            then unify t a 
            else put $ Data.Map.map (subAll t a) $ insert i a s
unify (TFunc fa fb) (TFunc ga gb) = unify fa ga >> unify fb gb
unify a b = throwError $ Mismatch a b
-}
