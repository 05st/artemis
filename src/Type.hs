{-# Language LambdaCase #-}
{-# Language PatternSynonyms #-}

module Type where

import Data.List

data Type = TCon String [Type] | TVar Int deriving (Eq, Ord)

pattern TBool = TCon "bool" []
pattern TInt = TCon "int" []
pattern TFloat = TCon "float" []
pattern TString = TCon "string" []
pattern TUnit = TCon "()" []
pattern TFunc a b = TCon "->" [a, b]

data Constraint = CEq Type Type deriving (Show)

instance Show Type where
    show = \case
        TFunc a b -> '(':show a ++ " -> " ++ show b ++ ")"
        TCon s p -> s ++ unwords (map show p)
        TVar i -> '$':show i
