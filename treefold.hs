import Data.Foldable as F

data Tree a = Empty
            | Leaf a
            | Bin (Tree a) a (Tree a)
            deriving (Show, Eq)

instance Foldable Tree where
    foldr _ z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Bin l x r) = F.foldr f (f x (F.foldr f z r)) l 
