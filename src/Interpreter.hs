{-# Language LambdaCase #-}

module Interpreter (typecheck, checkValue, checkExpr) where

import Control.Monad.Reader
import Control.Monad.Except

import AST
import Type

data TypeError = Mismatch Type Type | MismatchMult [Type] Type | NotFunction Type | NotDefined String | Unknown deriving (Show)
type TypeCheck a = a -> ExceptT TypeError (Reader TypeEnv) Type
type TypeEnv = [(String, Type)]

extend :: String -> Type -> TypeEnv -> TypeEnv
extend s t e = (s, t):e

numOp :: Type -> Type -> Either TypeError Type
numOp a b = case a of
    TInt -> case b of
        TInt -> Right TInt
        TFloat -> Right TFloat
        other -> Left $ MismatchMult [TInt, TFloat] other
    TFloat -> case b of
        TInt -> Right TFloat
        TFloat -> Right TFloat
        other -> Left $ MismatchMult [TInt, TFloat] other
    other -> Left $ MismatchMult [TInt, TFloat] other

typecheck :: [Stmt] -> Maybe TypeError
typecheck = undefined

checkStmt :: TypeCheck Stmt
checkStmt = undefined

checkExpr :: TypeCheck Expr
checkExpr = \case
    EValue v -> checkValue v
    EBlock stmts -> undefined
    EAssign lhs rhs -> do
        lt <- checkExpr lhs
        rt <- checkExpr rhs
        if lt == rt
            then return rt
            else throwError $ Mismatch lt rt
    EIf cond a b -> do
        ct <- checkExpr cond
        at <- checkExpr a
        bt <- checkExpr b
        if ct == TBool
            then if at == bt
                then return at
                else throwError $ Mismatch at bt
            else throwError $ Mismatch TBool ct
    ECall f p -> do
        pt <- checkExpr p
        ft <- checkExpr f
        case ft of
            (TFunc i o) -> if i == pt
                then return o
                else throwError $ Mismatch i pt
            other -> throwError $ NotFunction other
    EBinary op lhs rhs -> do
        lt <- checkExpr lhs
        rt <- checkExpr rhs
        case op of
            _ | op `elem` [Add, Sub, Mul, Div] ->
                case numOp lt rt of
                    (Left err) -> throwError err
                    (Right t) -> return t
            _ | op `elem` [Or, And] ->
                if lt /= TBool
                    then throwError $ Mismatch TBool lt
                    else if rt /= TBool
                        then throwError $ Mismatch TBool rt
                        else return TBool
            _ | op `elem` [Equal, NotEqual] -> return TBool
            _ | op `elem` [Greater, GreaterEqual, Lesser, LesserEqual] ->
                if (lt /= TInt) && (lt /= TFloat)
                    then throwError $ MismatchMult [TInt, TFloat] lt
                    else if (rt /= TInt) && (rt /= TFloat)
                        then throwError $ MismatchMult [TInt, TFloat] rt
                        else return TBool
            _ -> throwError Unknown
    EUnary op e -> do
        t <- checkExpr e
        case op of
            Sub ->
                if (t /= TInt) && (t /= TFloat)
                    then throwError $ MismatchMult [TInt, TFloat] t
                    else return t
            Not ->
                if t /= TBool
                    then throwError $ Mismatch TBool t
                    else return TBool
            _ -> throwError Unknown

checkValue :: TypeCheck Value
checkValue = \case
    VBool _ -> return TBool
    VInt _ -> return TInt
    VFloat _ -> return TFloat
    VString _ -> return TString
    VUnit -> return TUnit
    VFunc ft@(TFunc i o) p e -> do
        rt <- local (extend p i) (checkExpr e)
        if o == rt
            then return ft
            else throwError $ Mismatch o rt
    VIdent id -> do
        env <- ask
        case lookup id env of
            Just x -> return x
            Nothing -> throwError $ NotDefined id
    _ -> throwError Unknown
