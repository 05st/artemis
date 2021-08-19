{-# Language LambdaCase #-}

module Interpreter (interpret) where

import qualified Data.Map as Map

import Control.Monad.State
import Data.Functor

import System.IO

import Debug.Trace

import AST
import Type
import Value
import BuiltIn

interpret :: TProgram -> IO ()
interpret (Program ds) = evalStateT (evalProgram ds) defEnv

evalProgram :: [TDecl] -> StateT Env IO ()
evalProgram = foldr ((>>) . evalDecl) (return ())

evalDecl :: TDecl -> StateT Env IO ()
evalDecl = \case
    DStmt s -> evalStmt s
    DVar _ _ id v -> do
        e <- get
        v' <- evalExpr v
        case v' of
            VFunc (UserDef Nothing p b c) -> put (Map.insert id (VFunc (UserDef (Just id) p b c)) e)
            _ -> put (Map.insert id v' e)
    DData tc tps cs -> mapM_ valueConstructor cs

valueConstructor :: (String, [Type]) -> StateT Env IO ()
valueConstructor (vc, vts) = do
    env <- get
    let arity = length vts
    case vts of
        [] -> put (Map.insert vc (VData vc []) env)
        _ -> put (Map.insert vc (VFunc (BuiltIn arity [] (return . VData vc))) env)

evalStmt :: TStmt -> StateT Env IO ()
evalStmt = \case
    SExpr e -> void (evalExpr e)
    SPass _ -> error "Not possible"

evalExpr :: TExpr -> StateT Env IO Value
evalExpr = \case
    EInt _ n -> return $ VInt n
    EFloat _ n -> return $ VFloat n
    EBool _ b -> return $ VBool b
    EChar _ c -> return $ VChar c
    EUnit _ -> return VUnit
    EIdent _ id -> do
        env <- get
        case Map.lookup id env of
            Just v -> return v
            Nothing -> error $ "Undefined " ++ id ++ "\n\n" ++ show env
    EFunc _ p e -> get <&> VFunc . UserDef Nothing p e
    EIf _ c a b -> do
        c' <- evalExpr c
        let VBool cv = c'
        if cv then evalExpr a else evalExpr b
    EMatch _ e bs -> do
        e' <- evalExpr e
        let (p, be) = head $ dropWhile (\(p, _) -> not $ checkPattern e' p) bs
        setPatternVars e' p
        evalExpr be
    EBlock _ ds -> do
        orig <- get
        v <- evalBlock ds
        put orig
        return v
    ECall _ f a -> do
        f' <- evalExpr f
        a' <- evalExpr a
        let VFunc vf = f'
        case vf of
            UserDef n p e c -> do
                orig <- get
                case n of
                    Just id -> put (Map.insert p a' (Map.insert id f' c))
                    Nothing -> put (Map.insert p a' c)
                val <- evalExpr e
                put orig
                return val
            BuiltIn n args f -> do
                let args' = args ++ [a']
                if length args' == n
                    then liftIO (f args')
                    else return $ VFunc (BuiltIn n args' f)
    EAssign _ id v -> do
        e <- get
        v' <- evalExpr v
        put (Map.insert id v' e)
        return v'
    _ -> error "Not possible"

evalBlock :: [TDecl] -> StateT Env IO Value
evalBlock ((DStmt (SPass e)) : ds) = evalExpr e
evalBlock (d : ds) = evalDecl d >> evalBlock ds
evalBlock [] = error "No pass in block"

checkPattern :: Value -> Pattern -> Bool
checkPattern (VData dcon []) (PCon con []) = con == dcon
checkPattern (VData dcon vs) (PCon con ps) = (con == dcon) && or [checkPattern v p | (v, p) <- zip vs ps]
checkPattern _ (PVar _) = True
checkPattern _ _ = False

setPatternVars :: Value -> Pattern -> StateT Env IO ()
setPatternVars val (PVar var) = get >>= put . Map.insert var val
setPatternVars (VData dcon vs) (PCon con ps) = sequence_ [setPatternVars v p | (v, p) <- zip vs ps]
setPatternVars _ _ = return ()
