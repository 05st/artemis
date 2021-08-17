{-# Language LambdaCase #-}

module Interpreter where

import qualified Data.Map as Map

import Control.Monad.State
import Data.Functor

import AST
import Type

type Env = Map.Map Ident Value
data Value = VInt Integer | VFloat Double | VBool Bool | VChar Char | VUnit | VFunc Ident TExpr Env | VData Ident [Value]

evalDecl :: TDecl -> State Env ()
evalDecl = \case
    DStmt s -> evalStmt s
    DVar _ _ id v -> do
        e <- get
        v' <- evalExpr v
        put (Map.insert id v' e)
    DData tc tps cs -> return ()

evalStmt :: TStmt -> State Env ()
evalStmt = \case
    SExpr e -> void (evalExpr e)
    SPass _ -> error "Not possible"

evalExpr :: TExpr -> State Env Value
evalExpr = \case
    EInt _ n -> return $ VInt n
    EFloat _ n -> return $ VFloat n
    EBool _ b -> return $ VBool b
    EChar _ c -> return $ VChar c
    EUnit _ -> return VUnit
    EIdent _ id -> do
        env <- get
        let Just v = Map.lookup id env
        return v
    EFunc _ p e -> get <&> VFunc p e
    EIf _ c a b -> do
        c' <- evalExpr c
        let VBool cv = c'
        a' <- evalExpr a
        b' <- evalExpr b
        if cv then return a' else return b'
    EMatch _ e bs -> return $ VInt 0 -- TODO
    EBlock _ ds -> do
        orig <- get
        v <- evalBlock ds
        put orig
        return v
    ECall _ f a -> do
        f'<- evalExpr f
        let VFunc p e c = f'
        a' <- evalExpr a
        orig <- get
        let nenv = Map.insert p a' c
        put nenv
        val <- evalExpr e
        put orig
        return val
    EAssign _ id v -> do
        e <- get
        v' <- evalExpr v
        put (Map.insert id v' e)
        return v'
    _ -> error "Not possible"

evalBlock :: [TDecl] -> State Env Value
evalBlock ((DStmt (SPass e)) : ds) = evalExpr e
evalBlock (d : ds) = evalDecl d >> evalBlock ds
evalBlock [] = error "No pass in block"
