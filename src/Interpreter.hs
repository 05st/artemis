{-# Language LambdaCase #-}

module Interpreter (interpret) where

import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State

import System.IO

import Debug.Trace

import AST
import Type
import Value
import BuiltIn
import Name

type Interpret a = ReaderT (Namespace, [Namespace]) (StateT Env IO) a

interpret :: TProgram -> IO ()
interpret (Program ds) = evalStateT (runReaderT (evalProgram ds) (Global, [])) defEnv

evalProgram :: [TDecl] -> Interpret ()
evalProgram = foldr ((>>) . evalDecl) (return ())

evalDecl :: TDecl -> Interpret ()
evalDecl = \case
    DStmt s -> evalStmt s
    DVar _ _ id v -> do
        e <- get
        v' <- evalExpr v
        case v' of
            VFunc (UserDef Nothing p b c) -> put (Map.insert id (VFunc (UserDef (Just id) p b c)) e)
            _ -> put (Map.insert id v' e)
    DData tc tps cs -> mapM_ valueConstructor cs
    DNamespace name decls imps -> do
        (ns, _) <- ask
        local (const (Relative ns name, imps)) (foldr ((>>) . evalDecl) (return ()) decls)

valueConstructor :: (QualifiedName, [Type]) -> Interpret ()
valueConstructor (vc, vts) = do
    env <- get
    let arity = length vts
    case vts of
        [] -> put (Map.insert vc (VData vc []) env)
        _ -> put (Map.insert vc (VFunc (BuiltIn arity [] (return . VData vc))) env)

evalStmt :: TStmt -> Interpret ()
evalStmt = \case
    SExpr e -> void (evalExpr e)
    SPass _ -> error "Not possible"

evalLit :: Lit -> Value
evalLit = \case
    LInt n -> VInt n
    LFloat n -> VFloat n
    LBool b -> VBool b
    LChar c -> VChar c
    LUnit -> VUnit

evalExpr :: TExpr -> Interpret Value
evalExpr = \case
    ELit _ l -> return $ evalLit l
    EIdent _ name -> lookupEnv name
    EFunc _ p e -> do
        (ns, _) <- ask -- SHOULD BE RESOLVED ALREADY
        gets (VFunc . UserDef Nothing (Qualified ns p) e)
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

evalBlock :: [TDecl] -> Interpret Value
evalBlock ((DStmt (SPass e)) : ds) = evalExpr e
evalBlock (d : ds) = evalDecl d >> evalBlock ds
evalBlock [] = error "No pass in block"

checkPattern :: Value -> Pattern -> Bool
checkPattern (VData dcon []) (PCon con []) = con == dcon
checkPattern (VData dcon vs) (PCon con ps) = (con == dcon) && or [checkPattern v p | (v, p) <- zip vs ps]
checkPattern _ (PVar _) = True
checkPattern v (PLit l) = v == evalLit l
checkPattern _ _ = False

setPatternVars :: Value -> Pattern -> Interpret ()
setPatternVars val (PVar var) = do
    (ns, _) <- ask
    get >>= put . Map.insert (Qualified ns var) val
setPatternVars (VData dcon vs) (PCon con ps) = sequence_ [setPatternVars v p | (v, p) <- zip vs ps]
setPatternVars _ _ = return ()

------------
-- Lookup --
------------

lookupEnv :: QualifiedName -> Interpret Value
lookupEnv name = do
    res <- lookupEnv' name name
    case res of
        Nothing -> error $ "Undefined " ++ show name
        Just res' -> return res'

lookupEnv' :: QualifiedName -> QualifiedName -> Interpret (Maybe Value)
lookupEnv' name orig = do
    env <- get
    case Map.lookup name env of
        Just x -> return (Just x)
        Nothing -> case name of
            (Qualified Global _) -> ask >>= \(_, imps) -> lookupImports imps orig
            (Qualified (Relative parent _) s) -> lookupEnv' (Qualified parent s) orig

lookupImports :: [Namespace] -> QualifiedName -> Interpret (Maybe Value)
lookupImports [] _ = return Nothing
lookupImports (i : is) name@(Qualified ns s) = do
    let name' = Qualified i s
    res <- local (const (i, [])) (lookupEnv' name' name')
    case res of
        Just _ -> return res
        Nothing -> lookupImports is name
