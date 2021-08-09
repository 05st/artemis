module Main where

import System.Environment

import Parser
import Infer

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn . interpret . parse
    where
        interpret (Left err) = "ERROR: " ++ err
        interpret (Right stmts) =
            case typecheck stmts of
                Left err -> "ERROR: " ++ show err
                Right msg -> "Typecheck Passed\n\n" ++ msg
