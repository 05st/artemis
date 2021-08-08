{-# Language LambdaCase #-}
{-# Language PatternSynonyms #-}

module Type where

import Data.List

data Type = TCon String [Type] | TVar String deriving (Eq, Ord)
data Constraint = CEq Type Type deriving (Show)
data Scheme = Forall [Type] Type

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
        TCon s p -> s ++ '<':intercalate ", " (map show p) ++ ">"
        TVar s -> s
