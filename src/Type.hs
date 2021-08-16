{-# Language PatternSynonyms #-}
{-# Language LambdaCase #-}

module Type where

import Data.List
import Data.Set

data TVar = TV String Kind deriving (Show, Eq, Ord)
data Type = TCon String [Type] | TVar TVar deriving (Eq)
data Scheme = Forall (Set TVar) Type deriving (Show)

data Kind = Star | Kind :*> Kind deriving (Show, Eq, Ord)

data Constraint = Type :~: Type

pattern TInt = TCon "int" []
pattern TFloat = TCon "float" []
pattern TBool = TCon "bool" []
pattern TString = TCon "string" []
pattern TUnit = TCon "()" []
pattern TVoid = TCon "void" []
pattern a :-> b = TCon "->" [a, b]

instance Show Type where
    show = \case
        TVar (TV id _) -> id
        a :-> b -> '(':show a ++ " -> " ++ show b ++ ")"
        TCon c [] -> c
        TCon c ts -> c ++ '<':intercalate ", " (Prelude.map show ts) ++ ">"
