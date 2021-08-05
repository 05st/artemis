module Main where

import System.Environment

import Parser
import Interpreter

main :: IO ()
main = getArgs >>= putStrLn . interpret . run . head
    where
        interpret (Left err) = "ERROR: " ++ err
        interpret (Right stmts) =
            case typecheck stmts of
                Just err -> "ERROR: " ++ show err
                Nothing -> "Typecheck Passed " ++ (show stmts)
