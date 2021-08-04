{-# Language LambdaCase #-}

module Type where

import Data.List

data Type = TBool | TInt | TFloat | TString | TUnit | TFunc [Type] Type deriving (Eq)

instance Show Type where
    show = \case
        TBool -> "bool"
        TInt -> "int"
        TFloat -> "float"
        TString -> "string"
        TUnit -> "()"
        TFunc pts rt -> intercalate ", " (map show pts) ++ " -> " ++ show rt
