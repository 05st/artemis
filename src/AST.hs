{-# Language DeriveFunctor #-}

module AST where

import Type

type Ident = String
type Oper = String
type Mutable = Bool

newtype Program a = Program [Decl a] deriving (Show, Functor)
data Decl a = DStmt (Stmt a) | DVar Mutable Ident (Expr a) | DData Ident [TVar] [(Ident, [Type])] deriving (Show, Functor)
data Stmt a = SExpr (Expr a) | SPass (Expr a) deriving (Show, Functor)
data Expr a = EIdent a Ident | EInt a Integer | EFloat a Double | EBool a Bool | EChar a Char | EString a String | EUnit a | EFunc a Ident (Expr a)
            | EIf a (Expr a) (Expr a) (Expr a) | EMatch a (Expr a) [(Pattern, Expr a)] | EBlock a [Decl a]
            | EBinary a Oper (Expr a) (Expr a) | EUnary a Oper (Expr a) | EAssign a (Expr a) (Expr a) | ECall a (Expr a) (Expr a)
            deriving (Show, Functor)

type UProgram = Program ()
type UDecl = Decl ()
type UStmt = Stmt ()
type UExpr = Expr ()

type TProgram = Program Type
type TDecl = Decl Type
type TStmt = Stmt Type
type TExpr = Expr Type

data Pattern = PVar Ident | PCon Ident [Pattern] deriving (Show)
