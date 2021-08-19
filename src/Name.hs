module Name where

data Namespace = Global | Relative Namespace String deriving (Show, Eq, Ord)
data QualifiedName = Qualified Namespace String deriving (Show, Eq, Ord)

