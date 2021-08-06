{-# Language LambdaCase #-}

module AST where

import Type

type Program = [Stmt]
data Stmt = SExpr Expr | SPass Expr | SVar Type String Expr deriving (Show)
data Oper = Or | And | NotEqual | Equal | GreaterEqual | Greater | LesserEqual | Lesser | Add | Sub | Mul | Div | Not deriving (Eq, Show)
data Expr = EValue Value | EBlock [Stmt] | EAssign Expr Expr | EIf Expr Expr Expr | ECall Expr Expr | EBinary Oper Expr Expr | EUnary Oper Expr deriving (Show)
data Value = VIdent String | VString String | VBool Bool | VInt Integer | VFloat Double | VFunc Type String Expr | VUnit

instance Show Value where
    show = \case
        VIdent id -> id
        VString str -> '"' : str ++ "\""
        VBool bool -> if bool then "true" else "false"
        VInt int -> show int
        VFloat float -> show float
        a@(VFunc t _ _) -> "function " ++ show t
        VUnit -> "()"
