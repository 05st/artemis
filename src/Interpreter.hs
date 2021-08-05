{-# Language LambdaCase #-}

module Interpreter (typecheck, checkValue, checkExpr) where

import Control.Monad.Reader
import Control.Monad.Except

import AST
import Type

data TypeError = Mismatch Type Type | NotFunction Type | NotDefined String | Unknown deriving (Show)
type TypeCheck a = a -> ExceptT TypeError (Reader TypeEnv) Type
type TypeEnv = [(String, Type)]

extend :: String -> Type -> TypeEnv -> TypeEnv
extend s t e = (s, t):e

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
    ECall lhs rhs -> do
        
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
