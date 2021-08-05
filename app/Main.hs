module Main where

import System.Environment

import Parser
import Interpreter

main :: IO ()
main = getArgs >>= putStrLn . interpret . run . head
    where
        interpret (Left err) = "ERROR: " ++ err
        interpret (Right stmts) = show $ typecheck stmts
