{-# Language LambdaCase #-}

module AST where

type Program = [Stmt]
data Stmt = SExpr Expr | SRet Expr | SVar String Expr deriving (Show)
data Oper = Add | Sub | Mul | Div deriving (Show)
data Expr = EValue Value | EBlock [Stmt] | EAssign Expr Expr | EIf Expr Expr Expr | ECall Expr [Expr] | EBinary Expr Oper Expr | EUnary Oper Expr deriving (Show)
data Value = VIdent String | VString String | VBool Bool | VInt Integer | VFloat Double | VFunc [String] Expr | VUnit

instance Show Value where
    show = \case
        VIdent id -> id
        VString str -> '"' : str ++ "\""
        VBool bool -> if bool then "true" else "false"
        VInt int -> show int
        VFloat float -> show float
        VFunc _ _ -> "function"
        VUnit -> "()"

