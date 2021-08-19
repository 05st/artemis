module Value where

import qualified Data.Map as Map

import AST
import Name

type Env = Map.Map QualifiedName Value

data VFunc = UserDef (Maybe QualifiedName) String TExpr Env | BuiltIn Int [Value] ([Value] -> IO Value)
data Value = VInt Integer | VFloat Double | VBool Bool | VChar Char | VUnit | VFunc VFunc | VData String [Value] deriving (Show, Eq)

instance Show VFunc where
    show = const "func"

instance Eq VFunc where
    a == b = False
