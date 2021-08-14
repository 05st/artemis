{-# Language LambdaCase #-}

module AST where

import Data.List (intercalate)

import Type

type Program a = [Decl a]
data Decl a = DStmt (Stmt a) | DVar (Maybe Type) String (Expr a) | DData String [Type] [(String, [Type])] | DClass (Decl a) [Decl a] deriving (Show)
data Stmt a = SExpr (Expr a) | SPass (Expr a) deriving (Show)
data Expr a = EBlock a [Decl a] | EAssign a (Expr a) (Expr a) | EMatch a (Expr a) [(Pattern, Expr a)] | EIf a (Expr a) (Expr a) (Expr a) | ECall a (Expr a) (Expr a)
             | EBinary a BinOp (Expr a) (Expr a) | EUnary a UnaOp (Expr a)
             | EIdent a String | EString a String | EBool a Bool | EInt a Integer | EFloat a Double | EUnit a | EFunc a (Maybe Type) (Maybe Type) String (Expr a)
             deriving (Show)

type UntypedProgram = Program ()
type TypedProgram = Program Type
type UntypedDecl = Decl ()
type TypedDecl = Decl Type
type UntypedStmt = Stmt ()
type TypedStmt = Stmt Type
type UntypedExpr = Expr ()
type TypedExpr = Expr Type

data BinOp = Or | And | NotEqual | Equal | GreaterEqual | Greater | LesserEqual | Lesser | Add | Sub | Mul | Div | Exp deriving (Eq, Show)
data UnaOp = Neg | Not deriving (Show)

-- Value Constructor Matching (VC FuncName [Vars])
data Pattern = VC String [String] deriving (Show)

{-
instance Show Decl where
    show = \case
        DStmt s -> show s
        DVar _ id e -> "let " ++ id ++ " = " ++ show e ++ ";\n"
        DData tc ps vcs -> "data " ++ tc ++ '<':intercalate ", " (map show ps) ++ "> = " ++ intercalate " | " (map show vcs) ++ ";\n"

instance Show Stmt where
    show = \case
        SExpr e -> show e ++ ";\n"
        SPass e -> "pass " ++ show e ++ ";\n"

instance Show BinOp where
    show = \case
        Or -> "||"
        And -> "&&"
        NotEqual -> "!="
        Equal -> "=="
        GreaterEqual -> ">="
        Greater -> ">"
        LesserEqual -> "<="
        Lesser -> "<"
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Exp -> "^"

instance Show UnaOp where
    show = \case
        Neg -> "-"
        Not -> "!"

instance Show Expr where
    show = \case
        EBlock sts -> "{ " ++ unwords (map show sts) ++ " }"
        EAssign a b -> show a ++ " = " ++ show b
        EIf c a b -> "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
        ECall p f -> show f ++ '(':show p ++ ")"
        EBinary o l r -> show l ++ ' ':show o ++ ' ':show r
        EUnary o e -> show o ++ show e 
        EIdent id -> id
        EString str -> '"' : str ++ "\""
        EBool bool -> if bool then "true" else "false"
        EInt int -> show int
        EFloat float -> show float
        EFunc _ _ p e -> "fn(" ++ p ++ ") => " ++ show e
        EUnit -> "()"
        EMatch e ps -> "match " ++ show e ++ " with " ++ intercalate ", " (map show ps)
-}
