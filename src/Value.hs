module Value where

import qualified Data.Map as Map

import AST
import Name

type Env = Map.Map QualifiedName Value

data VFunc = UserDef Namespace [(QualifiedName, Value)] QualifiedName TExpr Env | BuiltIn Int [Value] ([Value] -> IO Value)
data Value = VInt Integer | VFloat Double | VBool Bool | VChar Char | VUnit | VFunc VFunc | VData QualifiedName [Value] deriving (Show, Eq)

instance Show VFunc where
    show (UserDef _ injects _ _ _) = "func" ++ ' ':show injects

instance Eq VFunc where
    a == b = False
