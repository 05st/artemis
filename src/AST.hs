{-# Language LambdaCase #-}

module AST where

import Type

import Data.List

type Program = [Stmt]
data Stmt = SExpr Expr | SPass Expr | SVar (Maybe Type) String Expr | SData String [Type] [(String, [Type])] deriving (Show)
data Expr = EBlock [Stmt] | EAssign Expr Expr | EIf Expr Expr Expr | ECall Expr Expr | EBinary Oper Expr Expr | EUnary Oper Expr
          | EIdent String | EString String | EBool Bool | EInt Integer | EFloat Double | EFunc (Maybe Type) (Maybe Type) String Expr | EUnit deriving (Show)

data Oper = Or | And | NotEqual | Equal | GreaterEqual | Greater | LesserEqual | Lesser | Add | Sub | Mul | Div | Exp | Not deriving (Eq, Show)
{-
instance Show Stmt where
    show = \case
        SExpr e -> show e ++ ";"
        SPass e -> "pass " ++ show e ++ ";"
        SVar _ id e -> "let " ++ id ++ " = " ++ show e ++ ";"
        SData tc ps vcs -> "data " ++ tc ++ '<':intercalate ", " (map show ps) ++ "> = " ++ intercalate " | " (map show vcs) ++ ";"

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
        Exp -> "^"

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
-}
