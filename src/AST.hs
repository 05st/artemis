{-# Language DeriveFunctor #-}

module AST where

import Type
import Name

type Oper = String
type Mutable = Bool

newtype Program a = Program [Decl a] deriving (Show, Functor)
data DVar a = DV Mutable (Maybe Type) QualifiedName (Expr a) deriving (Show, Functor)
data Decl a = DStmt (Stmt a)
            | DNamespace String [Decl a] [Namespace] 
            | DVar [DVar a]
            | DData QualifiedName [TVar] [(QualifiedName, [Type])] deriving (Show, Functor)
data Stmt a = SExpr (Expr a) | SPass (Expr a) deriving (Show, Functor)
data Expr a = EIdent a QualifiedName | ELit a Lit | EFunc a String (Expr a)
            | EIf a (Expr a) (Expr a) (Expr a) | EMatch a (Expr a) [(Pattern, Expr a)] | EBlock a [Decl a]
            | EBinary a Oper (Expr a) (Expr a) | EUnary a Oper (Expr a) | EAssign a QualifiedName (Expr a) | ECall a (Expr a) (Expr a)
            deriving (Show, Functor)
data Lit = LInt Integer | LFloat Double | LBool Bool | LChar Char | LUnit deriving (Eq, Show)

type UProgram = Program ()
type UDecl = Decl ()
type UDVar = DVar ()
type UStmt = Stmt ()
type UExpr = Expr ()

type TProgram = Program Type
type TDecl = Decl Type
type TDVar = DVar Type
type TStmt = Stmt Type
type TExpr = Expr Type

data Pattern = PVar String | PCon QualifiedName [Pattern] | PLit Lit deriving (Eq, Show)
