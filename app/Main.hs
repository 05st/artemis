module Main where

import qualified Data.Text.IO as TextIO
import System.Environment

import Parser

main :: IO ()
main = do
    file <- head <$> getArgs
    input <- TextIO.readFile file
    case parse input file of
        Left err -> putStrLn err
        Right program -> print program
