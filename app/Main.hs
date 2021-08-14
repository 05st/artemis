module Main where

import System.Environment

import Parser
import Analyze

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn . interpret . parse
    where
        interpret (Left err) = "ERROR: " ++ err
        interpret (Right decls) =
            case typecheck decls of
                Left err -> "ERROR: " ++ show err
                Right msg -> "Typecheck Passed\n\n" ++ msg ++ "\n\n" ++ show decls
