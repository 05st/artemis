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
    show (UserDef id e _) = id ++ " |fn|"
    show (BuiltIn n _ _) = show n ++ " |bi|"

-- Built-in functions
pAddInt [VInt a, VInt b] = return $ VInt (a + b)
pAddFloat [VFloat a, VFloat b] = return $ VFloat (a + b)
pPrint [a] = putStr (buildString a) >> return VUnit
pError [a] = error $ "ERROR: " ++ buildString a

-- Helper function
-- Turns a List<char> into a Haskell [Char]
buildString (VData "Elem" [VChar c, n]) = c : buildString n
buildString (VData "Empty" []) = []

defEnv :: Map.Map Ident Value
defEnv = Map.fromList [
    ("addInt", VFunc (BuiltIn 2 [] pAddInt)),
    ("addFloat", VFunc (BuiltIn 2 [] pAddFloat)),
    ("print", VFunc (BuiltIn 1 [] pPrint)),
    ("error", VFunc (BuiltIn 1 [] pError))]

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
            UserDef p e c -> do
                orig <- get
                let nenv = Map.insert p a' orig
                put nenv
                -- val <- evalExpr e
                --put orig
                -- return val
                evalExpr e
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
