{-# Language LambdaCase #-}
{-# Language PatternSynonyms #-}

module Type where

import Data.List
import Data.Set

newtype TVar = TV String deriving (Show, Eq, Ord)

data Type = TCon String [Type] | TVar TVar deriving (Eq, Ord)
data Constraint = Type :~ Type deriving (Show)
data Scheme = Forall (Set TVar) Type deriving (Eq, Show)

pattern TBool = TCon "bool" []
pattern TInt = TCon "int" []
pattern TFloat = TCon "float" []
pattern TString = TCon "string" []
pattern TUnit = TCon "()" []
pattern TVoid = TCon "void" []
pattern TFunc a b = TCon "->" [a, b]

instance Show Type where
    show = \case
        TFunc a b -> '(':show a ++ " -> " ++ show b ++ ")"
        TCon s [] -> s
        TCon s p -> s ++ '<':intercalate ", " (Prelude.map show p) ++ ">"
        TVar (TV s) -> s
