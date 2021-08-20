module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Environment
import System.FilePath.Posix (takeBaseName)

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
    inputs <- mapM TextIO.readFile args
    case parseFiles (zip files inputs) of
        Left err -> putStrLn err
        Right program -> case annotate (resolve program) of
            Left err -> print err
            Right annotated -> interpret annotated

{-
    case parse input (takeBaseName file) of
        Left err -> putStrLn err
        Right program -> print (resolve program) >>
            case annotate (resolve program) of
                Left err -> print err
                Right annotated -> interpret annotated
-}
