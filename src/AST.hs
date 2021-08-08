{-# Language LambdaCase #-}

module AST where

import Type

type Program = [Stmt]
data Stmt = SExpr Expr | SPass Expr | SVar (Maybe Type) String Expr
data Oper = Or | And | NotEqual | Equal | GreaterEqual | Greater | LesserEqual | Lesser | Add | Sub | Mul | Div | Not deriving (Eq)
data Expr = EItem Item | EBlock [Stmt] | EAssign Expr Expr | EIf Expr Expr Expr | ECall Expr Expr | EBinary Oper Expr Expr | EUnary Oper Expr
data Item = IIdent String | IString String | IBool Bool | IInt Integer | IFloat Double | IFunc (Maybe Type) (Maybe Type) String Expr | IUnit

instance Show Stmt where
    show = \case
        SExpr e -> show e ++ ";"
        SPass e -> "pass " ++ show e ++ ";"
        SVar _ id e -> "let " ++ id ++ " = " ++ show e ++ ";"

instance Show Oper where
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
        Not -> "!"

instance Show Expr where
    show = \case
        EItem i -> show i
        EBlock sts -> "{ " ++ unwords (map show sts) ++ " }"
        EAssign a b -> show a ++ " = " ++ show b
        EIf c a b -> "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
        ECall p f -> show f ++ '(':show p ++ ")"
        EBinary o l r -> show l ++ ' ':show o ++ ' ':show r
        EUnary o e -> show o ++ show e 

instance Show Item where
    show = \case
        IIdent id -> id
        IString str -> '"' : str ++ "\""
        IBool bool -> if bool then "true" else "false"
        IInt int -> show int
        IFloat float -> show float
        IFunc _ _ p e -> "fn(" ++ p ++ ") => " ++ show e
        IUnit -> "()"
