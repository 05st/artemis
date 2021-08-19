module Name where

data Namespace = Global | Relative Namespace String
data QualifiedName = Qualified [String] String deriving (Show, Eq, Ord)

lookupQualified :: QualifiedName -> Namespace -> a
lookupQualified (Qualified [] s) ns = lookupNamespace ns s
lookupQualified (Qualified (x : xs) s) ns = lookupQualified (Qualified xs s) (Relative ns x)

lookupNamespace = undefined

