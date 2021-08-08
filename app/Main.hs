module Main where

import System.Environment

import Parser
import Interpreter

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn . interpret . run
    where
        interpret (Left err) = "ERROR: " ++ err
        interpret (Right stmts) = 
            case typecheck stmts of
                Left err -> "ERROR: " ++ show err
                Right subst -> show stmts ++ "\n\n | Typecheck Passed | " ++ subst
