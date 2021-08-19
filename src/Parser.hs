module Parser (Parser.parse) where

import Data.Functor
import Data.Functor.Identity
import qualified Data.Text as Text

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Text (Parser)
import Text.Parsec.Language

import Debug.Trace

import Lexer
import AST
import Type
import Name

-------------
-- Program --
-------------

module' :: String -> Parser UDecl
module' file = do
    (is, ds) <- whitespace *> ((,) <$> imports <*> many declaration) <* whitespace <* eof
    return $ DNamespace file ds is

------------------
-- Declarations --
------------------

declaration :: Parser UDecl
declaration = (DStmt <$> statement) <|> varDecl <|> dataDecl <|> namespaceDecl

varDecl :: Parser UDecl
varDecl = do
    reserved "let"
    mut <- (True <$ reserved "mut") <|> (False <$ whitespace)
    id <- identifier <|> parens (operator <|> choice (map (\op -> reservedOp op >> return op) defOps))
    typeAnnotation <- option Nothing (Just <$> (colon *> type'))
    reservedOp "="
    expr <- expression
    semi
    return $ DVar mut typeAnnotation (Qualified Global id) expr

dataDecl :: Parser UDecl
dataDecl = do
    reserved "data"
    con <- Qualified Global <$> dataIdentifier
    tvars <- option [] (angles (sepBy (flip TV Star <$> identifier) comma))
    reservedOp "="
    vcons <- sepBy1 vcon (reservedOp "|")
    semi
    return $ DData con tvars vcons
    where
        vcon = do
            con <- Qualified Global <$> dataIdentifier
            tvars <- option [] (parens (sepBy type' comma))
            return (con, tvars)

namespaceDecl :: Parser UDecl
namespaceDecl = do
    reserved "namespace"
    id <- identifier
    (is, ds) <- braces ((,) <$> imports <*> many declaration)
    return $ DNamespace id ds is

imports :: Parser [Namespace]
imports = many (reserved "import" *> namespace <* semi)

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
    whitespace
    cond <- expression
    whitespace
    reserved "then"
    whitespace
    a <- expression
    whitespace
    reserved "else"
    whitespace
    EIf () cond a <$> expression

match :: Parser UExpr
match = do
    reserved "match"
    expr <- expression
    reserved "with"
    branches <- sepBy1 ((,) <$> pattern <*> (reservedOp "->" *> expression)) comma
    return $ EMatch () expr branches
    where
        pattern = try conPattern <|> litPattern <|> varPattern
        conPattern = PCon <$> (Qualified Global <$> dataIdentifier) <*> option [] (parens (sepBy1 pattern comma))
        varPattern = PVar <$> identifier
        litPattern = PLit <$> (int <|> float' <|> bool <|> char' <|> unit)

assign :: Parser UExpr
assign = do
    id <- identifier
    reservedOp "="
    EAssign () (Qualified Global id) <$> expression

call :: Parser UExpr
call = do
    id <- ident
    args <- parens (sepBy1 expression comma)
    return $ foldl1 (.) (flip (ECall ()) <$> reverse args) id

item :: Parser UExpr
item = try call <|> value <|> parens expression

int :: Parser Lit
int = LInt <$> (decimal <|> try octal <|> try hexadecimal)

float' :: Parser Lit
float' = LFloat <$> float

bool :: Parser Lit
bool = LBool <$> ((True <$ reserved "true") <|> (False <$ reserved "false"))

char' :: Parser Lit
char' = LChar <$> charLiteral

ident :: Parser UExpr
ident = EIdent () <$> qualified

unit :: Parser Lit
unit = LUnit <$ reserved "()"

function :: Parser UExpr
function = do
    reserved "fn"
    params <- parens (sepBy1 identifier comma) <?> "parameter"
    reservedOp "=>"
    expr <- expression
    return $ foldr (EFunc ()) (EFunc () (last params) expr) (init params)

-- Desugars a list of expressions into calls to Elem() and Empty
-- [e1, e2, e3, e4]
-- [ECall "Elem" e1, ECall "Elem" e2, ECall "Elem" e3, ECall "Elem" e4]
-- [ECall (ECall "Elem" e1), ECall (ECall "Elem" e2), ECall (ECall "Elem" e3), ECall (ECall "Elem" e4)]
-- then foldr into a single expression
desugarList :: [UExpr] -> Parser UExpr
desugarList exprs = do
    case exprs of
        [] -> return $ EIdent () (Qualified Global "Empty")
        _ -> return $ foldr (ECall () . ECall () (EIdent () (Qualified Global "Elem"))) (EIdent () (Qualified Global "Empty")) exprs

-- Regular list syntax sugar [e1, e2, e3]
list :: Parser UExpr
list = brackets (sepBy expression comma) >>= desugarList

-- String syntax sugar "abc!\n" "hello there123"
string' :: Parser UExpr
string' = stringLiteral >>= desugarList . map (ELit () . LChar)

value :: Parser UExpr
value = try function <|> try (ELit () <$> lit) <|> string' <|> ident <|> list

lit :: Parser Lit
lit = (try float' <|> try int) <|> bool <|> char' <|> unit

-----------
-- Types --
-----------

type' :: Parser Type
type' = try funcType <|> try conType <|> baseType

funcType :: Parser Type
funcType = do
    input <- try conType <|> baseType
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
        <|> (TBool <$ reserved "bool") <|> (TChar <$ reserved "char")
        <|> try (TUnit <$ reserved "()") <|> (TVoid <$ reserved "void")
        <|> (TVar <$> typeVar) <|> parens type'

-- Names
qualified :: Parser QualifiedName
qualified = do
    ids <- sepBy1 identifier (reservedOp "::")
    let ns = foldr (flip Relative) Global (reverse . init $ ids)
    return $ Qualified ns (last ids)

namespace :: Parser Namespace
namespace = do
    ids <- sepBy1 identifier (reservedOp "::")
    return $ foldr (flip Relative) Global (reverse ids)

---------
-- Run --
---------

parse :: Text.Text -> String -> Either String UDecl
parse input file =
    case Text.Parsec.parse (module' file) file input of
        Left err -> Left $ show err
        Right decls -> {- trace (show decls) $ -} Right decls
