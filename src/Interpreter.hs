{-# Language LambdaCase #-}

module Interpreter (interpret) where

import qualified Data.Map as Map

import Control.Monad.State
import Data.Functor

import Debug.Trace

import AST
import Type

type Env = Map.Map Ident Value
data VFunc = UserDef Ident TExpr Env | BuiltIn Int [Value] ([Value] -> IO Value)
data Value = VInt Integer | VFloat Double | VBool Bool | VChar Char | VUnit | VFunc VFunc | VData Ident [Value] deriving (Show)

instance Show VFunc where
    show _ = "func"

pAddInt :: [Value] -> IO Value
pAddInt [VInt a, VInt b] = return $ VInt (a + b)

pAddFloat :: [Value] -> IO Value
pAddFloat [VFloat a, VFloat b] = return $ VFloat (a + b)

pPrint :: [Value] -> IO Value
pPrint [a@(VData "Elem" [VChar c, n])] = putStr (buildString a) >> return VUnit
    where buildString (VData "Elem" [VChar c, n]) = c : buildString n
          buildString (VData "Empty" []) = []
pPrint other = print other >> return VUnit

defEnv :: Map.Map Ident Value
defEnv = Map.fromList [("addInt", VFunc (BuiltIn 2 [] pAddInt)), ("addFloat", VFunc (BuiltIn 2 [] pAddFloat)), ("print", VFunc (BuiltIn 1 [] pPrint))]

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
        put (Map.insert id v' e)
    DData tc tps cs -> do
        mapM_ valueConstructor cs

valueConstructor :: (Ident, [Type]) -> StateT Env IO ()
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
        let Just v = Map.lookup id env
        return v
    EFunc _ p e -> get <&> VFunc . UserDef p e
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
        f' <- evalExpr f
        a' <- evalExpr a
        let VFunc vf = f'
        case vf of
            UserDef p e c -> do
                orig <- get
                let nenv = Map.insert p a' c
                put nenv
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
