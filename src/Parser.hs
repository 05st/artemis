{-# Language TupleSections #-}

module Parser (Parser.parse) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as Text

import Data.List (nub)

import Text.Parsec hiding (State)
import Text.Parsec.Expr
import Text.Parsec.Language

import Debug.Trace

import Lexer
import AST
import Type
import Name

-- The Parser monad stack, Reader monad contains all of the imports so far.
-- All imports from parent namespaces "propagate" down
type Parser a = ParsecT Text.Text () (Reader [Namespace]) a

------------
-- Module --
------------
-- Parses a single module, aka file, and wraps it in a namespace with
-- the same name as the file.
module' :: String -> Parser UDecl
module' file = do
    whitespace
    imps <- imports
    declarations <- local (imps ++) (many declaration)
    whitespace *> eof
    return $ DNamespace file declarations imps

------------------
-- Declarations --
------------------
-- Parsing all of the types of declarations,
-- each declaration is either for variables, data types, namespaces, or statements.
declaration :: Parser UDecl
declaration = (DStmt <$> statement) <|> varDecl <|> dataDecl <|> namespaceDecl

-- Parse variable declaration (let x = <expr>;).
-- Mutually recursive declarations are separated by "and"
-- eg (let x = y; and y = x;)
varDecl :: Parser UDecl
varDecl = do
    reserved "let"
    decls <- sepBy1 var (reserved "and")
    return $ DVar decls

-- Helper function for parsing each variable declaration
-- separately.
var :: Parser UDVar
var = do
    isMut <- (True <$ reserved "mut") <|> (False <$ whitespace) -- if there is a 'mut' keyword following 'let', it is mutable
    id <- identifier <|> parens (operator <|> choice (map (\op -> reservedOp op >> return op) defOps))
    typeAnnotation <- option Nothing (Just <$> (colon *> type')) -- optional type annotation, only for variable declarations
    reservedOp "="
    expr <- expression
    semi
    return $ DV isMut typeAnnotation (Qualified Global id) expr
    -- this gets resolved to the correct namespace ^ during the resolver pass
    -- we don't want to be able to define a variable of some other namespace


-- Parse data declaration (data X = A | B | ...);
dataDecl :: Parser UDecl
dataDecl = do
    reserved "data"
    typeName <- Qualified Global <$> dataIdentifier -- dataIdentifiers must start with a capital letter
    typeParams <- option [] (angles (sepBy (flip TV Star <$> identifier) comma)) -- type parameters are just parsed as type variables
    reservedOp "="
    valueConstructors <- sepBy1 valueConstructor (reservedOp "|") -- parse value
    semi
    return $ DData typeName typeParams valueConstructors
    where
     -- valueConstructor :: Parser (QualifiedName, [Type])
        valueConstructor = do
            name <- Qualified Global <$> dataIdentifier
            types <- option [] (parens (sepBy type' comma)) -- parse fields of the value constructor
            return (name, types)

-- Parse namespace declaration (namespace xyz { <decls> })
namespaceDecl :: Parser UDecl
namespaceDecl = do
    reserved "namespace"
    name <- identifier
    -- each namespace starts with a list of imports, then any regular declarations after
    (imports, declarations) <- braces (do is <- imports ; (is,) <$> local (is++) (many declaration))
    parentImports <- ask -- these imports were passed on from parent namespaces
    let imports' = imports ++ parentImports
    return $ DNamespace name declarations (nub imports') -- nub: we only want unique imports

-- Parse 0 or more imports (import abc::def;)
imports :: Parser [Namespace]
imports = many (reserved "import" *> namespace <* semi)

----------------
-- Statements --
----------------
-- Parse a statement, either an expression statement or a pass statement
statement :: Parser UStmt
statement = (SExpr <$> expression <* semi) <|> passStmt

-- Parse a pass statement (pass <expr>;)
passStmt :: Parser UStmt
passStmt = SPass <$> (reserved "pass" *> expression <* semi)

-----------------
-- Expressions --
-----------------
-- Parsec operator table, contains pre-defined precedences for all of the built-in operators for now.
-- All user defined operators have the same precedence (for now).
-- This will be changed when user defined operators are able to have a precedence specified.
-- User defined operators can only be prefix and infix at the moment, no postfix.
opTable :: OperatorTable Text.Text () (Reader [Namespace]) UExpr
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

-- Parsers for any user defined operators
userPrefix = Prefix (EUnary () <$> operator)
userInfix = Infix (EBinary () <$> operator) AssocLeft

-- general expression parser
expression :: Parser UExpr
expression = buildExpressionParser (opTable ++ [[userPrefix], [userInfix]]) term
-- concatenate operator table with the table for user defined operators ^

-- Parse a term
term :: Parser UExpr
term = block <|> if' <|> match <|> try assign <|> item

-- Block expression ({ <decls> })
block :: Parser UExpr
block = EBlock () <$> braces (many declaration)

-- If expression (if <expr> then <expr> else <expr>)
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

-- Match expression (match <expr> with <branch>, ..., <branch>)
-- Branches separated by commas
-- Each branch is a pair (Pattern, Expr)
match :: Parser UExpr
match = do
    reserved "match"
    expr <- expression
    reserved "with"
    branches <- sepBy1 ((,) <$> pattern <*> (reservedOp "->" *> expression)) comma
    return $ EMatch () expr branches

-- Assignment expression (abc = <expr>);
assign :: Parser UExpr
assign = do
    id <- identifier
    reservedOp "="
    EAssign () (Qualified Global id) <$> expression

-- Function call expression. eg (abc(<expr>, ...));
-- Multiple arguments passed get desugared into multiple call expressions,
-- since every function is curried by default.
-- For example, abc(x, y, z) is sugar for ((abc(x))(y))(z)
call :: Parser UExpr
call = do
    id <- ident -- TODO: fix recursion problem, should be able to parse any expression and not just identifier for call
    args <- parens (sepBy1 expression comma)
    return $ foldl1 (.) (flip (ECall ()) <$> reverse args) id

item :: Parser UExpr
item = try call <|> value <|> parens expression

-- Integer literal
-- The lexer supports parsing in decimal (123), octal (o123), and hexadecimal (x123).
int :: Parser Lit
int = LInt <$> (decimal <|> try octal <|> try hexadecimal)

-- Floating point number literal
float' :: Parser Lit
float' = LFloat <$> float

-- Boolean literal
bool :: Parser Lit
bool = LBool <$> ((True <$ reserved "true") <|> (False <$ reserved "false"))

-- Character literal
char' :: Parser Lit
char' = LChar <$> charLiteral

-- Parses an identifier expression
ident :: Parser UExpr
ident = EIdent () <$> qualified identifier

-- Unit literal
unit :: Parser Lit
unit = LUnit <$ reserved "()"

-- Parses a function expression (regular functions or fnmatches)
-- Regular functions of multiple parameters automatically get desugared
-- into functions of single parameters, i.e. automatically curries them
-- For example, fn(a, b, c) => <expr>
-- gets desugared into: fn(a) => fn(b) => fn(c) => <expr>;
function :: Parser UExpr
function = fnmatch <|> do
    reserved "fn"
    params <- parens (sepBy1 identifier comma) <?> "parameter"
    reservedOp "=>"
    expr <- expression
    return $ foldr (EFunc ()) expr params

-- Desugars a fnmatch statement into a chain of fn(_) expressions followed by a system
-- of match expressions. The arity is determined by the number of patterns.
-- Example:
-- fnmatch
--     Test(a), Pat1(b) -> <expr>,
--     Test(a), pvar -> <expr>,
--     xy, Pat2(x, y) -> <expr>,
--     Test(b), Pat1(b) -> <expr>
-- desugars into:
-- fn(_a) => fn(_b) =>
--     (match _a with
--         Test(a) -> (match b with
--             Pat1(b) -> <expr>,
--             pvar -> <expr>),
--         xy -> (match b with
--             Pat2(x, y) -> <expr>))
fnmatch :: Parser UExpr
fnmatch = do
    reserved "fnmatch"
    branches <- sepBy1 parseBranch comma
    let depths@(arity : _) = map branchDepth branches
    if all (== arity) depths
        then do
            let expr = constructMatch . groupAll $ branches
            let (expr', n) = runState (fillMatchExpr "_a" expr) 1
            let params = take arity freeIdents
            let fn = foldr (EFunc ()) expr' params
            return fn
        else fail "all branches in fnmatch should be the same length"

-- This function fills out the identifiers in the match expression 
-- State monad keeps track of the index of the next identifier
fillMatchExpr :: String -> UExpr -> State Int UExpr
fillMatchExpr ident (EMatch t _ branches) = do
    let (patterns, exprs) = unzip branches
    nextIdent <- nextIdentifier -- fill all children match expressions with this identifier
    exprs' <- traverse (fillMatchExpr nextIdent) exprs
    let branches' = zip patterns exprs'
    return $ EMatch t (EIdent () (Qualified Global ident)) branches'
-- subtract 1 from state because we dont want to move up identifiers if it wasn't a match expression
fillMatchExpr _ other = get >>= put . subtract 1 >> return other

freeIdents :: [String]
freeIdents = map ('_':) ([1..] >>= flip replicateM ['a'..'z']) 

nextIdentifier :: State Int String
nextIdentifier = do
    n <- get
    put (n+1)
    return $ freeIdents !! n

-- Branch is a helper data type
-- Used in dealing with grouping the nested match expressions, for parsing fnmatches
data Branch = Branch Pattern [Branch] | Expr Pattern UExpr deriving (Show)

-- Used in figuring out the arity of the function desugared from fnmatches
branchDepth :: Branch -> Int
branchDepth (Expr _ _) = 1
branchDepth (Branch _ (child : _)) = 1 + branchDepth child
branchDepth _ = 0

-- Helper function
extractChilds :: Branch -> [Branch]
extractChilds (Expr _ _) = []
extractChilds (Branch _ childs) = childs

-- Helper function
checkBranch :: Pattern -> Branch -> Bool
checkBranch pat (Expr pat' _) = pat == pat'
checkBranch pat (Branch pat' _) = pat == pat'

-- Parses a single branch under the fnmatch expression
parseBranch :: Parser Branch
parseBranch = try nested <|> expr
    where
        nested = do
            pat <- pattern
            comma
            Branch pat . (:[]) <$> parseBranch
        expr = do
            pat <- pattern
            reservedOp "->"
            Expr pat <$> expression

-- Constructs a match expression given a list of branch data types
-- Done after "grouping" is done on the parsed branches
constructMatch :: [Branch] -> UExpr
constructMatch = EMatch () (EIdent () (Qualified Global "_")) . map constructMatch'
-- The identifier in the match expression gets filled in afterwards, so "_" is used temporarily

-- The main recursive function
constructMatch' :: Branch -> (Pattern, UExpr)
constructMatch' (Expr pat expr) = (pat, expr)
constructMatch' (Branch pat childs) =
    let match = constructMatch childs in (pat, match)

-- (all functions related to "grouping" below are to do with desugaring fnmatch expressions)
-- "Grouping" basically takes all consecutive branches with the same pattern,
-- and unifies them under one branch.
-- so instead of 
-- fnmatch
--     Pat1, Pat2 -> <expr>,
--     Pat1, Pat3 -> <expr>
-- turning to
-- match _a with
--     Pat1 -> match _b with
--         Pat2 -> <expr>,
--     Pat1 -> match _b with
--         Pat3 -> <expr>
-- (which is NOT what we want), it turns into
-- match _a with
--     Pat1 -> match _b with
--         Pat2 -> <expr>,
--         Pat3 -> <expr>
groupAll :: [Branch] -> [Branch]
groupAll = map groupChilds . groupBranches

-- Recursively groups children of a branch
groupChilds :: Branch -> Branch
groupChilds (Branch pat childs) = Branch pat (map groupChilds (groupBranches childs))
groupChilds other = other

-- Groups consecutive branches which match the same pattern.
-- It takes the first pattern as a reference, and puts all
-- consecutive similar branches (sharing patterns) as a child of
-- the reference branch.
groupBranches :: [Branch] -> [Branch]
groupBranches [] = []
groupBranches (a@(Expr _ _) : rest) = a : groupBranches rest
groupBranches (Branch pat childs : rest) =
    let (samePatterns, rest') = span (checkBranch pat) rest
    in Branch pat (childs ++ concatMap extractChilds samePatterns) : groupBranches rest'

-- Desugars a list of expressions into calls to Cons() and Empty
-- [e1, e2, e3, e4]
-- [ECall "Cons" e1, ECall "Cons" e2, ECall "Cons" e3, ECall "Cons" e4]
-- [ECall (ECall "Cons" e1), ECall (ECall "Cons" e2), ECall (ECall "Cons" e3), ECall (ECall "Cons" e4)]
-- then foldr into a single expression
-- Using this sugar syntax will throw a 'not defined' error if a list data type as below isn't defined:
-- List<a> = Cons(a, List<a>) | Empty
desugarList :: [UExpr] -> Parser UExpr
desugarList exprs = do
    case exprs of
        [] -> return $ EIdent () (Qualified (Relative Global "std") "Empty")
        _ -> return $ foldr (ECall () . ECall () (EIdent () (Qualified (Relative Global "std") "Cons"))) (EIdent () (Qualified (Relative Global "std") "Empty")) exprs

-- Regular list syntax sugar [e1, e2, e3]
list :: Parser UExpr
list = brackets (sepBy expression comma) >>= desugarList

-- String syntax sugar "abc!\n" "hello there123"
string' :: Parser UExpr
string' = stringLiteral >>= desugarList . map (ELit () . LChar)

-- Parses a 'value'
value :: Parser UExpr
value = try function <|> try (ELit () <$> lit) <|> string' <|> ident <|> list

-- Parses a literal
lit :: Parser Lit
lit = (try float' <|> try int) <|> bool <|> char' <|> unit

--------------
-- Patterns --
--------------
-- Parses a pattern
pattern :: Parser Pattern
pattern = (try conPattern <|> litPattern <|> varPattern) <* whitespace

-- Constructor pattern, used to match value constructors eg Pair(..., ...)
conPattern :: Parser Pattern
conPattern = PCon <$> (Qualified Global <$> dataIdentifier) <*> option [] (parens (sepBy1 pattern comma))

-- Variable pattern, matches anything
varPattern :: Parser Pattern
varPattern = PVar <$> identifier

-- Literal pattern, matches built-in literals
litPattern :: Parser Pattern
litPattern = PLit <$> (int <|> float' <|> bool <|> char' <|> unit)

-----------
-- Types --
-----------
-- Parse any type
-- These are only used in optional type annotations
type' :: Parser Type
type' = try funcType <|> try conType <|> baseType

-- Function type (a -> b), right associative
funcType :: Parser Type
funcType = do
    inputType <- try conType <|> baseType
    reservedOp "->"
    (inputType :->) <$> type'

-- Parses a type of the form Type<param1, param2, ...>
conType :: Parser Type
conType = do
    con <- qualified dataIdentifier
    typeParams <- option [] (angles (sepBy type' comma))
    return $ TCon con typeParams

-- Parses a type variable
typeVar :: Parser TVar
typeVar = do
    var <- identifier
    tps <- option [] (angles (sepBy typeVar comma))
    let kind = foldr ((:*>) . const Star) Star tps
    return $ TV var kind 

-- A 'basetype' is just a built-in type, or a type' surrounded by parenthesis
baseType :: Parser Type
baseType = (TInt <$ reserved "int") <|> (TFloat <$ reserved "float")
        <|> (TBool <$ reserved "bool") <|> (TChar <$ reserved "char")
        <|> try (TUnit <$ reserved "()") <|> (TVoid <$ reserved "void")
        <|> (TVar <$> typeVar) <|> parens type'

-----------
-- Names --
-----------
-- Parse a qualified name, something like abc::def::xyz
-- parses into Qualified (Relative (Relative Global "abc") "def") "xyz"
qualified :: Parser String -> Parser QualifiedName
qualified p = do
    ids <- sepBy1 p (reservedOp "::")
    let namespace = foldr (flip Relative) Global (reverse . init $ ids)
    return $ Qualified namespace (last ids)

-- Essentially the same as parsing a qualified name, except only namespaces
-- Something like abc::def::xyz parses into
-- Relative (Relative (Relative Global "abc") "def") "xyz"
namespace :: Parser Namespace
namespace = do
    ids <- sepBy1 identifier (reservedOp "::")
    return $ foldr (flip Relative) Global (reverse ids)

---------
-- Run --
---------
-- Run the the module parser on Text input. Second parameter is file name (without extensions).
-- Returns either an error (String), or the parsed namespace declaration of the module.
parse :: Text.Text -> String -> Either String UDecl
parse input file =
    case runReader (runParserT (module' file) () file input) [] of
        Left err -> Left $ show err
        Right decls -> {- trace (show decls) $ -} Right decls
