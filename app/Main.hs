module Main where

import System.Environment

import Parser

main :: IO ()
main = getArgs >>= putStrLn . run . head
