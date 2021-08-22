{-# Language LambdaCase #-}

module Interpreter (interpret) where

import qualified Data.Map as Map
import Data.Foldable
import Data.Bifunctor

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

import System.IO

import Debug.Trace

import AST
import Type
import Value
import BuiltIn
import Name

type Interpret a = ReaderT Namespace (StateT (Env, Map.Map Namespace [Namespace]) IO) a

interpret :: TProgram -> IO ()
interpret (Program ds) = evalStateT (runReaderT (evalProgram ds) Global) (defEnv, Map.empty)

evalProgram :: [TDecl] -> Interpret ()
evalProgram = foldr ((>>) . evalDecl) (return ())

evalDecl :: TDecl -> Interpret ()
evalDecl = \case
    DStmt s -> evalStmt s
    DVar decls -> do
        (names, values) <- unzip <$> traverse evalVarDecl decls
        let toInject = zip names values
        let values' = map (fillVFunc toInject) values
        traverse_ (\(name, value) -> do
            (env, m) <- get
            put (Map.insert name value env, m)) (zip names values')
    DData tc tps cs -> mapM_ valueConstructor cs
    DNamespace name decls imps -> do
        ns <- ask
        (e, imap) <- get
        let newns = Relative ns name
        put (e, Map.insert newns imps imap)
        local (const newns) (foldr ((>>) . evalDecl) (return ()) decls)

fillVFunc :: [(QualifiedName, Value)] -> Value -> Value
fillVFunc injects (VFunc (UserDef namespace _ param expr closure)) = VFunc (UserDef namespace injects param expr closure)
fillVFunc _ other = other

evalVarDecl :: TDVar -> Interpret (QualifiedName, Value)
evalVarDecl (DV _ _ name expr) = (,) name <$> evalExpr expr

valueConstructor :: (QualifiedName, [Type]) -> Interpret ()
valueConstructor (vc, vts) = do
    (env, m) <- get
    let arity = length vts
    case vts of
        [] -> put (Map.insert vc (VData vc []) env, m)
        _ -> put (Map.insert vc (VFunc (BuiltIn arity [] (return . VData vc))) env, m)

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
        ns <- ask -- SHOULD BE RESOLVED ALREADY
        (env, _) <- get
        return . VFunc $ UserDef ns [] (Qualified ns p) e env
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
            UserDef ns toInject p e c -> do
                (orig, m) <- get
                let toInject' = map (second (fillVFunc toInject)) toInject
                let c' = c `Map.union` Map.fromList toInject'
                put (Map.insert p a' c', m)
                val <- local (const ns) (evalExpr e)
                (_, m') <- get
                put (orig, m')
                return val
            BuiltIn n args f -> do
                let args' = args ++ [a']
                if length args' == n
                    then liftIO (f args')
                    else return $ VFunc (BuiltIn n args' f)
    EAssign _ id v -> do
        (e, m) <- get
        v' <- evalExpr v
        put (Map.insert id v' e, m)
        return v'
    _ -> error "Not possible"

evalBlock :: [TDecl] -> Interpret Value
evalBlock ((DStmt (SPass e)) : ds) = evalExpr e
evalBlock (d : ds) = evalDecl d >> evalBlock ds
evalBlock [] = error "No pass in block"

checkPattern :: Value -> Pattern -> Bool
checkPattern (VData (Qualified _ dcon) []) (PCon (Qualified _ con) []) = con == dcon
checkPattern (VData (Qualified _ dcon) vs) (PCon (Qualified _ con) ps) = (con == dcon) && and [checkPattern v p | (v, p) <- zip vs ps]
checkPattern _ (PVar _) = True
checkPattern v (PLit l) = v == evalLit l
checkPattern _ _ = False

setPatternVars :: Value -> Pattern -> Interpret ()
setPatternVars val (PVar var) = do
    ns  <- ask
    (env, m) <- get
    put (Map.insert (Qualified ns var) val env, m)
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
    (env, m) <- get
    case Map.lookup name env of
        Just x -> return (Just x)
        Nothing -> case name of
            (Qualified Global _) -> do
                ns <- ask
                let imps = fromMaybe [] (Map.lookup ns m)
                lookupImports imps orig
            (Qualified (Relative parent _) s) -> lookupEnv' (Qualified parent s) orig

lookupImports :: [Namespace] -> QualifiedName -> Interpret (Maybe Value)
lookupImports [] _ = return Nothing
lookupImports (i : is) name@(Qualified ns s) = do
    let name' = Qualified i s
    res <- local (const i) (lookupEnv' name' name')
    case res of
        Just _ -> return res
        Nothing -> lookupImports is name
