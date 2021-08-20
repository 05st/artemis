{-# OPTIONS_GHC -fdefer-type-errors #-}

module BuiltIn (defTEnv, defEnv) where

import Data.Functor
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Time
import Data.Time.Clock.POSIX
import System.IO

import Type
import Value
import Name

-- Built-in functions, type checker guarantees that these patterns are matched
addInt [VInt a, VInt b] = return $ VInt (a + b)
subInt [VInt a, VInt b] = return $ VInt (a - b)
mulInt [VInt a, VInt b] = return $ VInt (a * b)
divInt [VInt a, VInt b] = return $ VInt (a `div` b)
floor' [VFloat a] = return $ VInt (floor a)

addFloat [VFloat a, VFloat b] = return $ VFloat (a + b)
subFloat [VFloat a, VFloat b] = return $ VFloat (a - b)
mulFloat [VFloat a, VFloat b] = return $ VFloat (a * b)
divFloat [VFloat a, VFloat b] = return $ VFloat (a / b)
fromInt [VInt a] = return $ VFloat (fromIntegral a)

eqInt [VInt a, VInt b] = return $ VBool (a == b)
neqInt [VInt a, VInt b] = return $ VBool (a /= b)

showInt [VInt a] = return $ fromString (show a)
showFloat [VFloat a] = return $ fromString (show a)
showBool [VBool a] = return $ fromString $ if a then "true" else "false"
showChar' [VChar a] = return $ fromString (show a)
showUnit [VUnit] = return $ fromString "()"

readInt [a] = return $ VInt (read $ toString a)
readFloat [a] = return $ VFloat (read $ toString a)

print' [a] = putStr (toString a) >> hFlush stdout >> return VUnit
error' [a] = Prelude.error $ "ERROR: " ++ toString a
clock [a] = getCurrentTime <&> VInt . floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

input [a] = getLine <&> fromString


-- Helper function
-- Turns a List<char> into a Haskell [Char]
toString :: Value -> String
toString (VData "Elem" [VChar c, n]) = c : toString n
toString (VData "Empty" []) = []
toString _ = error "Not possible"

fromString :: String -> Value
fromString (c : cs) = VData "Elem" [VChar c, fromString cs]
fromString [] = VData "Empty" []

builtIn :: String -> ([Value] -> IO Value) -> Int -> [TVar] -> Type -> (String, Value, Scheme, Bool)
builtIn name fn arity vs t = (name, VFunc (BuiltIn arity [] fn), Forall (Set.fromList vs) t, False)

toQualified :: String -> QualifiedName
toQualified = Qualified Global

builtIns :: [(String, Value, Scheme, Bool)]
builtIns = [
        builtIn "addInt" addInt 2 [] (TInt :-> (TInt :-> TInt)),
        builtIn "subInt" subInt 2 [] (TInt :-> (TInt :-> TInt)),
        builtIn "mulInt" mulInt 2 [] (TInt :-> (TInt :-> TInt)),
        builtIn "divInt" divInt 2 [] (TInt :-> (TInt :-> TInt)),
        builtIn "floor" floor' 1 [] (TFloat :-> TInt),

        builtIn "addFloat" addFloat 2 [] (TFloat :-> (TFloat :-> TFloat)),
        builtIn "subFloat" subFloat 2 [] (TFloat :-> (TFloat :-> TFloat)),
        builtIn "mulFloat" mulFloat 2 [] (TFloat :-> (TFloat :-> TFloat)),
        builtIn "divFloat" divFloat 2 [] (TFloat :-> (TFloat :-> TFloat)),
        builtIn "fromInt" fromInt 1 [] (TInt :-> TFloat),

        builtIn "eqInt" eqInt 2 [] (TInt :-> (TInt :-> TBool)),

        builtIn "showInt" showInt 1 [] (TInt :-> TList TChar),
        builtIn "showFloat" showFloat 1 [] (TFloat :-> TList TChar),
        builtIn "showBool" showBool 1 [] (TBool :-> TList TChar),
        builtIn "showChar" showChar' 1 [] (TChar :-> TList TChar),
        builtIn "showUnit" showUnit 1 [] (TUnit :-> TList TChar),

        builtIn "readInt" readInt 1 [] (TList TChar :-> TInt),
        builtIn "readFloat" readFloat 1 [] (TList TChar :-> TFloat),

        builtIn "print" print' 1 [] (TList TChar :-> TUnit),
        builtIn "error" error' 1 [TV "a" Star] (TList TChar :-> TVar (TV "a" Star)),
        builtIn "clock" clock 1 [] (TUnit :-> TInt),

        builtIn "input" input 1 [] (TUnit :-> TList TChar)
    ]

defTEnv :: TEnv
defTEnv = Map.fromList $ map (\(i, _, s, m) -> (toQualified i, (s, m))) builtIns

defEnv :: Env
defEnv = Map.fromList $ map (\(i, v, _, _) -> (toQualified i, v)) builtIns
