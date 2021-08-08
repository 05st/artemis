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

data TypeError = Mismatch Type Type | NotFunction Type | NotDefined String | EmptyBlock | PassOutOfBlock | UnifyError Type Type | InfiniteType Type Type | Unknown deriving (Show)

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
addConst a b = tell [CEq a b]

typecheck :: [Stmt] -> Either TypeError Subst
typecheck stmts = case runExcept (runRWST (checkProgram stmts) [] (Data.Map.empty, 0)) of
    Left err -> Left err
    Right (_, (s, _), c) -> runSolve c s

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
        addConst TBool ct
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

inferItem :: Item -> Infer Type
inferItem = \case
    IIdent s -> do
        env <- ask
        case Prelude.lookup s env of
            Nothing -> throwError $ NotDefined s
            Just t -> return t
    IString _ -> return TString
    IBool _ -> return TBool
    IInt _ -> return TInt
    IFloat _ -> return TFloat
    IFunc pM rM p e -> do
        pt <- freshMaybe pM
        rt <- freshMaybe rM
        et <- local (extend p pt) (inferExpr e)
        addConst rt et
        return (TFunc pt rt)
    IUnit -> return TUnit

substitute :: Subst -> Type -> Type
substitute s = \case
    TCon ss ps -> TCon ss (Prelude.map (substitute s) ps)
    a@(TVar i) -> fromMaybe a (Data.Map.lookup i s)

tvs :: Type -> Set Type
tvs (TCon _ ps) = Prelude.foldr (Data.Set.union . tvs) Data.Set.empty ps
tvs a@(TVar _) = Data.Set.singleton a

occurs :: Type -> Type -> Bool
occurs a b = a `Data.Set.member` tvs b

unify :: Type -> Type -> ExceptT TypeError (State Subst) ()
unify a b | a == b = return ()
unify (TVar i) b = bind i b 
unify a (TVar i) = bind i a
unify (TFunc fa fb) (TFunc ga gb) = unify fa ga >> unify fb gb
unify a b = throwError $ Mismatch a b

bind :: Int -> Type -> ExceptT TypeError (State Subst) ()
bind i t | t == a = return ()
         | occurs a t = throwError $ InfiniteType a t
         | otherwise = get >>= put . Data.Map.insert i t
    where a = TVar i

solve :: [Constraint] -> ExceptT TypeError (State Subst) Subst
solve ((CEq a b) : cs) = do
    s <- get
    unify (substitute s a) (substitute s b)
    s' <- get
    put (Data.Map.map (substitute s') s')
    solve cs
solve [] = get

runSolve :: [Constraint] -> Subst -> Either TypeError Subst
runSolve c s = case runState (runExceptT (solve c)) s of
    (Left err, _) -> Left err
    (Right _, s) -> Right s
