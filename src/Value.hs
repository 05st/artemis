module Value where

import qualified Data.Map as Map

import AST

type Env = Map.Map String Value

data VFunc = UserDef String TExpr Env | BuiltIn Int [Value] ([Value] -> IO Value)
data Value = VInt Integer | VFloat Double | VBool Bool | VChar Char | VUnit | VFunc VFunc | VData String [Value]
