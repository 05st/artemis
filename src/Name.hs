module Name where

data Namespace = Global | Relative Namespace String deriving (Eq, Ord)
data QualifiedName = Qualified Namespace String deriving (Eq, Ord)

instance Show Namespace where
    show Global = []
    show (Relative parent name) = show parent ++ "::" ++ name

instance Show QualifiedName where
    show (Qualified namespace name) = show namespace ++ "::" ++ name
