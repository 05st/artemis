{-# Language PatternSynonyms #-}
{-# Language LambdaCase #-}

module Type where

import Data.List
import Data.Set
import qualified Data.Map as Map
import Name

type TEnv = Map.Map QualifiedName (Scheme, Bool)

data TVar = TV String Kind deriving (Eq, Ord)
data Type = TCon QualifiedName [Type] | TVar TVar deriving (Eq)
data Scheme = Forall (Set TVar) Type deriving (Show)

data Kind = Star | Kind :*> Kind deriving (Show, Eq, Ord)

data Constraint = Type :~: Type

pattern TInt = TCon (Qualified Global "int") []
pattern TFloat = TCon (Qualified Global "float") []
pattern TBool = TCon (Qualified Global "bool") []
pattern TChar = TCon (Qualified Global "char") []
pattern TUnit = TCon (Qualified Global "()") []
pattern TVoid = TCon (Qualified Global "void") []
pattern a :-> b = TCon (Qualified Global "->") [a, b]
pattern TList a = TCon (Qualified (Relative Global "std") "List") [a]

instance Show Type where
    show = \case
        TVar tv -> show tv
        a :-> b -> '(':show a ++ " -> " ++ show b ++ ")"
        TCon (Qualified _ c) [] -> c
        TCon (Qualified _ c) ts -> c ++ '<':intercalate ", " (Prelude.map show ts) ++ ">"

instance Show TVar where
    show (TV s _) = s

instance Show Constraint where
    show (a :~: b) = show a ++ " ~ " ++ show b
