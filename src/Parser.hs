module Parser (Parser.parse) where

import Data.Functor
import Data.Functor.Identity
import qualified Data.Text as Text

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as Token

import Debug.Trace

import AST
import Type

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
    , Token.reservedOpNames = defOps ++ ["->", "=>", "|", "?"]
    , Token.caseSensitive = True }

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
operator = Token.operator lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
colon = Token.colon lexer
whitespace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer
dot = Token.dot lexer
angles = Token.angles lexer
brackets = Token.brackets lexer

dataIdentifier = (:) <$> upper <*> identifier

-------------
-- Program --
-------------

program :: Parser UProgram
program = Program <$> (whitespace *> many declaration <* whitespace <* eof)

------------------
-- Declarations --
------------------

declaration :: Parser UDecl
declaration = (DStmt <$> statement) <|> varDecl <|> dataDecl

varDecl :: Parser UDecl
varDecl = do
    reserved "let"
    mut <- (True <$ reserved "mut") <|> (False <$ whitespace)
    id <- identifier <|> parens (operator <|> choice (map (\op -> reservedOp op >> return op) defOps))
    reservedOp "="
    DVar mut id <$> expression <* semi

dataDecl :: Parser UDecl
dataDecl = do
    reserved "data"
    con <- dataIdentifier
    tvars <- option [] (angles (sepBy (flip TV Star <$> identifier) comma))
    reservedOp "="
    vcons <- sepBy1 vcon (reservedOp "|")
    semi
    return $ DData con tvars vcons
    where
        vcon = do
            con <- dataIdentifier
            tvars <- option [] (parens (sepBy type' comma))
            return (con, tvars)

----------------
-- Statements --
----------------

statement :: Parser UStmt
statement = (SExpr <$> expression <* semi) <|> passStmt

passStmt :: Parser UStmt
passStmt = SPass <$> (reserved "pass" *> expression <* semi)

-----------------
-- Expressions --
-----------------

opTable :: OperatorTable Text.Text () Identity UExpr
opTable = 
    [[prefixOp "-", prefixOp "!"],
    -- [postfixOp "?"],
    [infixOp "^" AssocRight],
    [infixOp "*" AssocLeft, infixOp "/" AssocLeft],
    [infixOp "+" AssocLeft, infixOp "-" AssocLeft],
    [infixOp ">" AssocLeft, infixOp "<" AssocLeft, infixOp ">=" AssocLeft, infixOp "<=" AssocLeft],
    [infixOp "==" AssocLeft, infixOp "!=" AssocLeft],
    [infixOp "&&" AssocLeft],
    [infixOp "||" AssocLeft]]
    where 
        prefixOp op = Prefix (reservedOp op >> return (EUnary () op))
        infixOp op = Infix (reservedOp op >> return (EBinary () op))
        postfixOp op = Postfix (reservedOp op >> return (EUnary () op))

userPrefix = Prefix (operator <&> EUnary ())
userInfix = Infix (operator <&> EBinary ()) AssocLeft

expression :: Parser UExpr
expression = buildExpressionParser (opTable ++ [[userPrefix], [userInfix]]) term

term :: Parser UExpr
term = block <|> if' <|> match <|> try assign <|> item

block :: Parser UExpr
block = EBlock () <$> braces (many declaration)

if' :: Parser UExpr
if' = do
    reserved "if"
    cond <- expression
    reserved "then"
    a <- expression
    reserved "else"
    EIf () cond a <$> expression

match :: Parser UExpr
match = do
    reserved "match"
    expr <- expression
    reserved "with"
    branches <- sepBy1 ((,) <$> pattern <*> (reservedOp "->" *> expression)) comma
    return $ EMatch () expr branches
    where
        pattern = try conPattern <|> varPattern
        conPattern = PCon <$> dataIdentifier <*> option [] (parens (sepBy1 pattern comma))
        varPattern = PVar <$> identifier

assign :: Parser UExpr
assign = do
    id <- identifier
    reservedOp "="
    EAssign () (EIdent () id) <$> expression

call :: Parser UExpr
call = do
    id <- identifier
    args <- parens (sepBy1 expression comma)
    return $ foldl1 (.) (flip (ECall ()) <$> reverse args) (EIdent () id)

item :: Parser UExpr
item = try call <|> value <|> parens expression

int :: Parser UExpr
int = EInt () <$> integer

float :: Parser UExpr
float = do
    w <- integer
    dot
    f <- integer
    return $ EFloat () (read (show w ++ '.':show f))

bool :: Parser UExpr
bool = EBool () <$> ((True <$ reserved "true") <|> (False <$ reserved "false"))

string' :: Parser UExpr
string' = EString () <$> (char '"' *> many (noneOf "\"") <* char '"')

ident :: Parser UExpr
ident = EIdent () <$> identifier

unit :: Parser UExpr
unit = EUnit () <$ reserved "()"

function :: Parser UExpr
function = do
    reserved "fn"
    params <- parens (sepBy1 identifier comma) <?> "parameter"
    reservedOp "=>"
    expr <- expression
    return $ foldr (EFunc ()) (EFunc () (last params) expr) (init params)

-- Desugars a list value into calls to Elem() and Empty
-- [e1, e2, e3, e4]
-- [ECall "Elem" e1, ECall "Elem" e2, ECall "Elem" e3, ECall "Elem" e4]
-- [ECall (ECall "Elem" e1), ECall (ECall "Elem" e2), ECall (ECall "Elem" e3), ECall (ECall "Elem" e4)]
-- ECall (ECall "Elem" e3) (ECall (ECall "Elem" e4) Empty)
list :: Parser UExpr
list = do
    items <- brackets (sepBy expression comma)
    case items of
        [] -> return $ EIdent () "Empty"
        _ -> return $ foldr (ECall () . ECall () (EIdent () "Elem")) (EIdent () "Empty") items

value :: Parser UExpr
value = try function <|> (try float <|> int) <|> bool <|> string' <|> ident <|> unit <|> list

-----------
-- Types --
-----------

type' :: Parser Type
type' = try funcType <|> try conType <|> baseType

funcType :: Parser Type
funcType = do
    input <- baseType
    reservedOp "->"
    (input :->) <$> type'

conType :: Parser Type
conType = do
    con <- dataIdentifier
    tps <- option [] (angles (sepBy type' comma))
    return $ TCon con tps

typeVar :: Parser TVar
typeVar = do
    var <- identifier
    tps <- option [] (angles (sepBy typeVar comma))
    let kind = foldr ((:*>) . const Star) Star tps
    return $ TV var kind 

baseType :: Parser Type
baseType = (TInt <$ reserved "int") <|> (TFloat <$ reserved "float")
        <|> (TBool <$ reserved "bool") <|> (TString <$ reserved "string")
        <|> try (TUnit <$ reserved "()") <|> (TVoid <$ reserved "void")
        <|> (TVar <$> typeVar) <|> parens type'

---------
-- Run --
---------

parse :: Text.Text -> String -> Either String UProgram
parse input filename =
    case Text.Parsec.parse program filename input of
        Left err -> Left $ show err
        Right decls -> {- trace (show decls) $ -} Right decls
