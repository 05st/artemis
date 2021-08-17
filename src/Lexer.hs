{-# Language OverloadedStrings #-}

module Lexer where

import Data.Functor.Identity
import qualified Data.Text as Text

import Text.Parsec
import qualified Text.Parsec.Token as Token

-----------
-- Lexer --
-----------

defOps :: [String]
defOps = ["+", "-", "*", "/", "^", "=", "==", "!=", ">", ">=", "<", "<=", "!", "&&", "||"] 

lexer :: Token.GenTokenParser Text.Text () Identity
lexer = Token.makeTokenParser $ Token.LanguageDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.nestedComments = True
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> oneOf "_'"
    , Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.opLetter =  oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.reservedNames = ["fn", "true", "false", "let", "mut", "pass", "int", "float", "bool", "string", "()", "void", "if", "then", "else", "match", "with", "data"]
    , Token.reservedOpNames = defOps ++ ["->", "=>", "|"]
    , Token.caseSensitive = True }

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
operator = Token.operator lexer
parens = Token.parens lexer
-- integer = Token.integer lexer
decimal = Token.decimal lexer
octal = Token.octal lexer
hexadecimal = Token.hexadecimal lexer
float = Token.float lexer
semi = Token.semi lexer
colon = Token.colon lexer
whitespace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer
dot = Token.dot lexer
angles = Token.angles lexer
brackets = Token.brackets lexer
charLiteral = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer

dataIdentifier = (:) <$> upper <*> identifier
