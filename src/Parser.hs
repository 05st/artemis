{-# Language TupleSections #-}

module Parser (run) where

-- TODO: call expressions only allow identifiers as the function expression, so (test(a))(b) doesn't work. fix

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator

import Data.Maybe

import AST
import Type

keywords :: [String]
keywords = ["fn", "true", "false", "let", "pass", "bool", "int", "string", "float", "if", "else", "then", "void"]

identStr :: Parser String
identStr = do
    first <- letter <|> char '_'
    rest <- many (letter <|> digit <|> oneOf "_'")
    let str = first:rest
    if str `elem` keywords
        then unexpected ("keyword '" ++ str ++ "'") <?> "valid identifier"
        else return str

tIdentStr :: Parser String
tIdentStr = do
    first <- upper
    rest <- many (letter <|> digit <|> oneOf "_'")
    return $ first:rest

reqSpaces :: Parser ()
reqSpaces = skipMany1 space

-- Program
program :: Parser Program
program = many (spaces *> statement <* spaces)

-- Statements
statement :: Parser Stmt
statement = dataStmt <|> varStmt <|> exprStmt <|> passStmt

dataStmt :: Parser Stmt
dataStmt = do
    string "data" *> reqSpaces
    con <- tIdentStr <* spaces
    tvars' <- (char '<' *> spaces *> tvars <* spaces <* char '>' <* spaces) <|> ([] <$ spaces)
    spaces *> char '=' *> spaces
    vcons <- sepBy1 vcon (spaces *> char '|' *> spaces)
    spaces *> char ';'
    return $ SData con tvars' vcons
    where
        vcon = do
            con <- tIdentStr
            tvars' <- (spaces *> char '(' *> spaces *> tvars <* spaces <* char ')' <* spaces) <|> ([] <$ spaces)
            return (con, tvars')
        tvars = sepBy pTVar (spaces *> char ',' *> spaces)

passStmt :: Parser Stmt
passStmt = SPass <$> (string "pass" *> reqSpaces *> expression <* spaces <* char ';')

varStmt :: Parser Stmt
varStmt = do
    string "let" *> reqSpaces
    id <- identStr
    t <- try (Just <$> (spaces *> char ':' *> spaces *> pType <* spaces)) <|> (Nothing <$ spaces)
    char '=' *> spaces
    SVar t id <$> expression <* spaces <* char ';'

exprStmt :: Parser Stmt
exprStmt = SExpr <$> expression <* spaces <* char ';'

-- Expressions

expression :: Parser Expr
expression = block <|> try if' <|> try assign <|> logicOr

block :: Parser Expr
block = EBlock <$> (char '{' *> many (spaces *> statement <* spaces) <* char '}')

assign :: Parser Expr
assign = do
    id <- ident
    EAssign (EItem id) <$> (spaces *> char '=' *> spaces *> expression)

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
        return $ foldl1 (.) (ECall <$> reverse args) (EItem fnexpr)) <|> item

item :: Parser Expr
item = try valExpr <|> (char '(' *> expression <* char ')')

valExpr :: Parser Expr
valExpr = EItem <$> value

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

-- Items

ident :: Parser Item
ident = IIdent <$> identStr

string' :: Parser Item
string' = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return $ IString content

bool :: Parser Item
bool = IBool <$> ((True <$ string "true") <|> (False <$ string "false"))

integer :: Parser Item
integer = IInt . read <$> many1 digit

float :: Parser Item
float = do
    ints <- many1 digit
    dot <- char '.'
    frac <- many1 digit
    return $ IFloat . read $ ints ++ dot:frac

parameter :: Parser (String, Maybe Type)
parameter = do
    ident <- identStr
    pt <- (Just <$> (spaces *> char ':' *> spaces *> pType)) <|> (Nothing <$ spaces)
    return (ident, pt)

function :: Parser Item
function = do
    string "fn" *> spaces
    char '('
    pts <- (spaces *> sepBy1 parameter (spaces *> char ',' *> spaces)) <?> "parameter"
    char ')'
    rt <- try (Just <$> (spaces *> string "->" *> spaces *> pType)) <|> (Nothing <$ spaces)
    spaces *> string "=>" *> spaces
    expr <- expression
    case pts of
        [(p, t)] -> return $ IFunc t rt p expr
        other -> do
            let (params, types) = unzip pts
            if (Nothing `elem` types) || isNothing rt
                then return $ foldr1 (.) [IFunc i Nothing p . EItem | (p, i) <- init pts] (IFunc (last types) rt (last params) expr)
                else let utypes = map fromJust (types ++ [rt]) 
                     in let funcTypes = [(head dropped, foldr1 TFunc (tail dropped)) | i <- [0 .. length utypes - 2], let dropped = drop i utypes]
                        in return $ foldr1 (.) [IFunc (Just i) (Just o) p . EItem | ((i, o), p) <- init (zip funcTypes params)] (IFunc (Just $ fst (last funcTypes)) (Just $ snd (last funcTypes)) (last params) expr)

value :: Parser Item
value = try function <|> string' <|> try bool <|> ident <|> (try float <|> integer) <|> (IUnit <$ string "()")

-- Types

pType :: Parser Type
pType = try pTFunc <|> try pTCon <|> pTItem

pTFunc :: Parser Type
pTFunc = do 
    input <- pTItem
    spaces *> string "->" *> spaces
    TFunc input <$> pType

pTCon :: Parser Type
pTCon = do
    con <- tIdentStr
    tps <- (spaces *> char '<' *> spaces *> sepBy pType (spaces *> char ',' *> spaces) <* spaces <* char '>' <* spaces) <|> ([] <$ spaces)
    return $ TCon con tps

pTItem :: Parser Type
pTItem = try pTLit <|> pTVar <|> (char '(' *> spaces *> pType <* spaces <* char ')')

pTVar :: Parser Type
pTVar = TVar <$> identStr

pTLit :: Parser Type
pTLit = (TBool <$ string "bool") <|> (TInt <$ string "int") <|> (TFloat <$ string "float") <|> (TString <$ string "string")
        <|> try (TUnit <$ string "()") <|> (TVoid <$ string "void") -- <|> (TCon <$> (spaces *> identStr <* spaces) <*> (spaces *> sepBy pType spaces <* spaces))

--

run :: String -> Either String [Stmt]
run input = case parse program "artemis" input of
    Left err -> Left $ show err
    Right val -> Right val
