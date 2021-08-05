{-# Language TupleSections #-}

module Parser (run) where

-- TODO: call expressions only allow identifiers as the function expression, so (test(a))(b) doesn't work. fix

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator

import AST
import Type

keywords :: [String]
keywords = ["fn", "true", "false", "let", "pass", "bool", "int", "string", "float"]

identStr :: Parser String
identStr = do
    first <- letter <|> char '_'
    rest <- many (letter <|> digit <|> oneOf "_'")
    let str = first:rest
    if str `elem` keywords
        then unexpected ("keyword '" ++ str ++ "'") <?> "valid identifier"
        else return str

reqSpaces :: Parser ()
reqSpaces = skipMany1 space

-- Program
program :: Parser Program
program = sepBy statement spaces

-- Statements
statement :: Parser Stmt
statement = varStmt <|> exprStmt <|> passStmt

passStmt :: Parser Stmt
passStmt = SPass <$> (string "pass" *> reqSpaces *> expression <* spaces <* char ';')

varStmt :: Parser Stmt
varStmt = do
    string "let" *> spaces
    id <- identStr
    spaces *> char '=' *> spaces
    SVar id <$> expression <* spaces <* char ';'

exprStmt :: Parser Stmt
exprStmt = SExpr <$> expression <* spaces <* char ';'

-- Expressions

expression :: Parser Expr
expression = block <|> try assign <|> logicOr <|> if'

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
call = try (do
    fnexpr <- ident
    char '('
    args <- (spaces *> sepBy1 expression (spaces *> char ',' *> spaces)) <?> "argument"
    char ')'
    return $ foldl1 (.) (ECall <$> reverse args) (EValue fnexpr)) <|> item

item :: Parser Expr
item = try valExpr <|> (char '(' *> expression <* char ')')

valExpr :: Parser Expr
valExpr = EValue <$> value

-- Operators

binaryOps :: Parser Expr -> [(Oper, String)] -> Parser Expr
binaryOps next opmap = let flist = [op <$ string lex | (op, lex) <- opmap]
    in chainl1 (spaces *> next <* spaces) (EBinary <$> foldr1 ((<|>) . try) flist) 

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
unary = try call <|> (do
    op <- (Sub <$ char '-') <|> (Not <$ char '!')
    EUnary op <$> unary)

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
    char '('
    pts <- (spaces *> sepBy1 parameter (spaces *> char ',' *> spaces)) <?> "parameter"
    char ')' *> spaces
    string "->" *> spaces
    rt <- pType
    spaces *> string "=>" *> spaces
    expr <- expression
    case pts of
        [(p, t)] -> return $ VFunc (TFunc t rt) p expr
        other -> do
            let (params, types) = unzip pts
            let funcTypes = [foldr1 TFunc (drop i types ++ [rt]) | i <- [0 .. length types - 1]]
            return $ foldr1 (.) [VFunc funcType param . EValue | (funcType, param) <- init (zip funcTypes params)] (VFunc (last funcTypes) (last params) expr)

value :: Parser Value
value = function <|> string' <|> bool <|> ident <|> (try float <|> integer) <|> (VUnit <$ string "()")

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
run input = case parse program "artemis" input of
    Left err -> "ERROR: " ++ show err
    Right val -> show val
