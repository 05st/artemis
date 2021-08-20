module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Environment
import System.FilePath.Posix (takeBaseName)

import Debug.Trace

import Parser
import Resolver
import Infer
import Interpreter
import AST

parseFiles :: [(String, Text.Text)] -> Either String UProgram
parseFiles files = do
    modules <- mapM (\(file, input) -> parse input (takeBaseName file)) files
    Right (Program modules)

main :: IO ()
main = do
    args <- getArgs
    let files = map takeBaseName args
    putStrLn $ "Reading " ++ show files
    inputs <- mapM TextIO.readFile args
    putStrLn $ "Parsing " ++ show files
    case parseFiles (zip files inputs) of
        Left err -> putStrLn $ "Parse Error: " ++ err
        Right program -> do
            putStrLn "Resolving names"
            let resolved = resolve program
            putStrLn "Inferring types"
            case annotate resolved of
                Left err -> putStrLn $ "Type Error: " ++ show err
                Right annotated -> do
                    putStrLn "Interpreting program"
                    interpret annotated
