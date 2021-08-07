{-# Language LambdaCase #-}

module AST where

import Type

type Program = [Stmt]
data Stmt = SExpr Expr | SPass Expr | SVar (Maybe Type) String Expr deriving (Show)
data Oper = Or | And | NotEqual | Equal | GreaterEqual | Greater | LesserEqual | Lesser | Add | Sub | Mul | Div | Not deriving (Eq, Show)
data Expr = EItem Item | EBlock [Stmt] | EAssign Expr Expr | EIf Expr Expr Expr | ECall Expr Expr | EBinary Oper Expr Expr | EUnary Oper Expr deriving (Show)
data Item = IIdent String | IString String | IBool Bool | IInt Integer | IFloat Double | IFunc (Maybe Type) (Maybe Type) String Expr | IUnit deriving (Show)

{-
instance Show Value where
    show = \case
        VIdent id -> id
        VString str -> '"' : str ++ "\""
        VBool bool -> if bool then "true" else "false"
        VInt int -> show int
        VFloat float -> show float
        VFunc {} -> "function" --function " ++ show t
        VUnit -> "()"
-}
