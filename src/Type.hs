{-# Language LambdaCase #-}

module Type where

import Data.List

data Type = TCon String | TVar Int | TFunc Type Type deriving (Eq, Ord)

data Constraint = CEquality Type Type deriving (Show)

tBool :: Type
tBool = TCon "bool"

tInt :: Type
tInt = TCon "int"

tFloat :: Type
tFloat = TCon "float"

tString :: Type
tString = TCon "string"

tUnit :: Type
tUnit = TCon "()"

instance Show Type where
    show = \case
        TCon s -> s
        TVar i -> '$':show i
        TFunc a b -> '(':show a ++ " -> " ++ show b ++ ")"
