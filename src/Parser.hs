{-# Language TupleSections #-}

module Parser (run) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator

import AST
import Type

identStr :: Parser String
identStr = do
    first <- letter
    rest <- many (letter <|> digit <|> oneOf "_'")
    return $ first:rest

reqSpaces :: Parser ()
reqSpaces = skipMany1 space

-- Statements
statement :: Parser Stmt
statement = exprStmt

exprStmt :: Parser Stmt
exprStmt = SExpr <$> expression

-- Expressions

expression :: Parser Expr
expression = block <|> assign <|> if' <|> logicOr

block :: Parser Expr
block = EBlock <$> (char '{' *> many (spaces *> statement <* spaces) <* char '}')

assign :: Parser Expr
assign = do
    id <- ident
    EAssign (EValue id) <$> (spaces *> char '=' *> spaces *> expression)

if' :: Parser Expr
if' = do
    string "if" *> reqSpaces
    cond <- expression
    spaces *> string "then" *> reqSpaces
    a <- expression
    spaces *> string "else" *> reqSpaces
    EIf cond a <$> expression

call :: Parser Expr
call = undefined

valExpr :: Parser Expr
valExpr = EValue <$> value

-- Operators

binaryOps :: Parser Expr -> [(Oper, String)] -> Parser Expr
binaryOps next opmap = let flist = [op <$ string lex | (op, lex) <- opmap]
    in chainl1 next (EBinary <$> foldr1 (<|>) flist) 

logicOr :: Parser Expr
logicOr = binaryOps logicAnd [(Or, "||")]

logicAnd :: Parser Expr
logicAnd = binaryOps equality [(And, "&&")]

equality :: Parser Expr
equality = binaryOps comparison [(NotEqual, "!="), (Equal, "==")]

comparison :: Parser Expr
comparison = binaryOps term [(GreaterEqual, ">="), (Greater, ">"), (LesserEqual, "<="), (Lesser, "<")]

term :: Parser Expr
term = binaryOps factor [(Sub, "-"), (Add, "+")]

factor :: Parser Expr
factor = binaryOps unary [(Div, "/"), (Mul, "*")]

unary :: Parser Expr
unary = (do
    op <- (Sub <$ char '-') <|> (Not <$ char '!')
    EUnary op <$> unary) <|> call

-- Values

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
    (ident,) <$> pType

function :: Parser Value
function = do
    string "fn" *> spaces
    char '(' *> spaces
    char ')' *> spaces
    string "->" *> spaces
    pType
    string "=>" *> spaces
    return $ VFunc (TFunc TBool TInt) "a" (EBlock [])

value :: Parser Value
value = string' <|> bool <|> ident  <|> (try float <|> integer) <|> function <|> (VUnit <$ string "()")

-- Types

pType :: Parser Type
pType = try f <|> pBaseType
    where
    f = do 
        input <- pBaseType
        spaces *> string "->" *> spaces
        TFunc input <$> pType

pBaseType :: Parser Type
pBaseType = try pLitType <|> (char '(' *> spaces *> pType <* spaces <* char ')')

pLitType :: Parser Type
pLitType = (TBool <$ string "bool") <|> (TInt <$ string "int") <|> (TFloat <$ string "float") <|> (TString <$ string "string") <|> try (TUnit <$ string "()")

--

run :: String -> String
run input = case parse statement "artemis" input of
    Left err -> "ERROR: " ++ show err
    Right val -> show val
