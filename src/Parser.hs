{-# Language TupleSections #-}

module Parser (run) where

import Text.Parsec
import Text.Parsec.String (Parser)

import AST
import Type

ptype :: Parser Type
ptype = undefined

identStr :: Parser String
identStr = do
    first <- letter
    rest <- many (letter <|> digit <|> oneOf "_'")
    return $ first:rest

ident :: Parser Value
ident = VIdent <$> identStr

string' :: Parser Value
string' = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return $ VString content

bool :: Parser Value
bool = VBool <$> ((True <$ string "true") <|> (False <$ string "false"))

integer :: Parser Value
integer = VInt . read <$> many1 digit

float :: Parser Value
float = do
    ints <- many1 digit
    dot <- char '.'
    frac <- many1 digit
    return $ VFloat . read $ ints ++ dot:frac

parameter :: Parser (String, Type)
parameter = do
    ident <- identStr
    spaces *> char ':' *> spaces
    (ident,) <$> ptype

function :: Parser Value
function = do
    string "fn" *> spaces
    char '(' *> spaces
    (params, types) <- unzip <$> sepBy parameter (spaces *> char ',' *> spaces)
    spaces *> char ')' *> spaces
    string "->" *> spaces
    rtype <- ptype
    spaces *> string "=>" *> spaces
    VFunc (TFunc types rtype) params <$> (block <|> expression)

value :: Parser Value
value = string' <|> bool <|> ident  <|> (try float <|> integer) <|> function <|> (VUnit <$ string "()")

block = undefined

expression = undefined

run :: String -> String
run input = case parse value "artemis" input of
    Left err -> "ERROR: " ++ show err
    Right val -> show val
