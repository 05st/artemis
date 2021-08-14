{-# Language TupleSections #-}

module Parser (Parser.parse) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language

import Data.Maybe
import Data.Functor.Identity

import AST
import Type

-----------
-- Lexer --
-----------

rKeywords :: [String]
rKeywords = ["fn", "true", "false", "let", "pass", "bool", "int", "string", "float", "if", "else", "then", "match", "with", "void", "data", "()"]

rOperators :: [String]
rOperators = ["+", "-", "*", "/", "^", "=", "==", "!=", ">", ">=", "<", "<=", "!", "&&", "||", "->", "=>", "|"]

languageDef =
    emptyDef { Token.commentStart = "/*"
             , Token.commentEnd = "*/"
             , Token.commentLine = "//"
             , Token.nestedComments = True
             , Token.identStart = letter
             , Token.identLetter = alphaNum <|> oneOf "_'"
             , Token.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
             , Token.reservedNames = rKeywords
             , Token.reservedOpNames = rOperators
             , Token.caseSensitive = True
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
colon = Token.colon lexer
whitespace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer
dot = Token.dot lexer
angles = Token.angles lexer

-------------
-- Program --
-------------

program :: Parser UntypedProgram
program = whitespace *> many declaration <* whitespace <* eof

------------------
-- Declarations --
------------------

declaration :: Parser UntypedDecl
declaration = (DStmt <$> statement) <|> varDecl <|> dataDecl

varDecl :: Parser UntypedDecl
varDecl = do
    reserved "let"
    var <- identifier
    t <- (Just <$> typeAnnotation) <|> (Nothing <$ whitespace)
    reservedOp "="
    DVar t var <$> expression <* semi

dataDecl :: Parser UntypedDecl
dataDecl = do
    reserved "data"
    con <- identifier
    tvars <- angles (sepBy typeVar comma <|> ([] <$ whitespace))
    reservedOp "="
    vcons <- sepBy1 vcon (reservedOp "|")
    semi
    return $ DData con tvars vcons
    where
        vcon = do
            con <- identifier
            tvars <- parens (sepBy type' comma) <|> ([] <$ whitespace)
            return (con, tvars)

----------------
-- Statements --
----------------

statement :: Parser UntypedStmt
statement = (SExpr <$> (expression <* semi)) <|> passStmt

passStmt :: Parser UntypedStmt
passStmt = SPass <$> (reserved "pass" *> expression <* semi)

-----------------
-- Expressions --
-----------------

term :: Parser UntypedExpr
term = block <|> if' <|> match <|> try assign <|> item

block :: Parser UntypedExpr
block = EBlock () <$> braces (many declaration)

if' :: Parser UntypedExpr
if' = do
    reserved "if"
    cond <- expression
    reserved "then"
    a <- expression
    reserved "else"
    EIf () cond a <$> expression

pattern :: Parser Pattern
pattern = do
    con <- identifier
    vars <- parens (sepBy1 identifier comma) <|> ([] <$ whitespace)
    return $ VC con vars

match :: Parser UntypedExpr
match = do
    reserved "match"
    expr <- expression
    reserved "with"
    branches <- sepBy1 ((,) <$> pattern <*> (reservedOp "->" *> expression)) comma
    return $ EMatch () expr branches

assign :: Parser UntypedExpr
assign = do
    ident <- identifier
    reservedOp "="
    EAssign () (EIdent () ident) <$> expression

call :: Parser UntypedExpr
call = do
    fn <- identifier
    args <- parens (sepBy1 expression comma)
    return $ foldl1 (.) (flip (ECall ()) <$> reverse args) (EIdent () fn)

item :: Parser UntypedExpr
item = try call <|> value <|> parens expression

float :: Parser UntypedExpr
float = do
    whole <- integer
    dot
    frac <- integer
    return $ EFloat () (read (show whole ++ show frac))

bool :: Parser UntypedExpr
bool = (EBool () True <$ reserved "true") <|> (EBool () False <$ reserved "false")

parameter :: Parser (String, Maybe Type)
parameter = do
    ident <- identifier
    pt <- (Just <$> (colon *> type')) <|> (Nothing <$ whitespace)
    return (ident, pt)

function :: Parser UntypedExpr
function = do
    reserved "fn"
    pts <- parens (sepBy1 parameter comma) <?> "parameter"
    rt <- try (Just <$> (reservedOp "->" *> type')) <|> (Nothing <$ whitespace)
    reservedOp "=>"
    expr <- expression
    case pts of
        [(p, t)] -> return $ EFunc () t rt p expr
        _ -> do
            let (params, types) = unzip pts
            if (Nothing `elem` types) || isNothing rt
                then return $ foldr1 (.) [EFunc () i Nothing p | (p, i) <- init pts] (EFunc () (last types) rt (last params) expr)
                else let utypes = map fromJust (types ++ [rt])
                     in let funcTypes = [(head dropped, foldr1 TFunc (tail dropped)) | i <- [0 .. length utypes - 2], let dropped = drop i utypes]
                        in return $ foldr1 (.) [EFunc () (Just i) (Just o) p | ((i, o), p) <- init (zip funcTypes params)] (EFunc () (Just $ fst (last funcTypes)) (Just $ snd (last funcTypes)) (last params) expr) 

value :: Parser UntypedExpr
value = try function <|> (try float <|> (EInt () <$> integer)) <|> bool <|> (EString () <$> (char '"' *> many (noneOf "\"") <* char '"')) <|> (EIdent () <$> identifier) <|> (EUnit () <$ reserved "()")

---------------
-- Operators --
---------------

opTable :: OperatorTable String () Identity UntypedExpr
opTable = [
        [Prefix (reservedOp "-" >> return (EUnary () Neg)),
         Prefix (reservedOp "!" >> return (EUnary () Not))],

        [Infix (reservedOp "^" >> return (EBinary () Exp)) AssocRight],
    
        [Infix (reservedOp "*" >> return (EBinary () Mul)) AssocLeft,
         Infix (reservedOp "/" >> return (EBinary () Div)) AssocLeft],

        [Infix (reservedOp "+" >> return (EBinary () Add)) AssocLeft,
         Infix (reservedOp "-" >> return (EBinary () Sub)) AssocLeft],

        [Infix (reservedOp ">" >> return (EBinary () Greater)) AssocLeft,
         Infix (reservedOp "<" >> return (EBinary () Lesser)) AssocLeft,
         Infix (reservedOp ">=" >> return (EBinary () GreaterEqual)) AssocLeft,
         Infix (reservedOp "<=" >> return (EBinary () LesserEqual)) AssocLeft],

        [Infix (reservedOp "==" >> return (EBinary () Equal)) AssocLeft,
         Infix (reservedOp "!=" >> return (EBinary () NotEqual)) AssocLeft],

        [Infix (reservedOp "&&" >> return (EBinary () And)) AssocLeft],

        [Infix (reservedOp "||" >> return (EBinary () Or)) AssocLeft]
    ]

expression :: Parser UntypedExpr
expression = buildExpressionParser opTable term

-----------
-- Types --
-----------

typeAnnotation :: Parser Type
typeAnnotation = colon *> type'

type' :: Parser Type
type' = try funcType <|> try conType <|> baseType

funcType :: Parser Type
funcType = do
    input <- baseType
    reservedOp "->"
    TFunc input <$> type'

conType :: Parser Type
conType = do
    con <- identifier
    tps <- angles (sepBy type' comma <|> ([] <$ whitespace))
    return $ TCon con tps

typeVar :: Parser Type
typeVar = TVar . TV <$> identifier

baseType :: Parser Type
baseType = (TBool <$ reserved "bool") <|> (TInt <$ reserved "int") <|> (TFloat <$ reserved "float") <|> (TString <$ reserved "string")
       <|> try (TUnit <$ reserved "()") <|> (TVoid <$ reserved "void") <|> typeVar <|> parens type'

---------
-- Run --
---------

parse :: String -> Either String UntypedProgram
parse input = case Text.Parsec.parse program "artemis" input of
    Left err -> Left $ show err
    Right decls -> Right decls
