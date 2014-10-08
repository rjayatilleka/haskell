import Data.List (intercalate, tails)

data List a = EmptyList
            | Cons a (List a)
            deriving (Show)


myDrop :: Int -> [b] -> [b]
myDrop n xs =
    if n <= 0 || null xs
    then xs
    else myDrop (n - 1) (tail xs)

data Animal = Human String
            | Other String
            deriving (Show)

showAnimal :: Animal -> String
showAnimal (Human a) = "Lord " ++ a
showAnimal (Other a) = a

commafy :: [String] -> String
commafy = intercalate ", "

filterEvens :: [Int] -> [Int]
filterEvens = filter isEven
    where isEven :: Int -> Bool
          isEven = (==0) . (`mod` 2)

pairs :: [a] -> [(a, a)]
pairs list = concat $ zipWith (zip . repeat) list $ tails list

-- [1, 2, 3]
-- [(1, [1, 2, 3]),
--  (2,    [2, 3]),
--  (3,       [3])]
-- list , tails list
-- zip list (tails list)
--
-- (1, [1, 2, 3]) -> [(1, 1), (1, 2), (1, 3)]
-- \(x, ys) -> zip (repeat x) ys
-- map (\(x, ys) -> zip (repeat x) ys) $ zip list (tails list)
-- zipWith (\x ys -> zip (repeat x) ys) list (tails list)
-- zipWith (\x -> zip (repeat x)) list (tails list)
-- zipWith (zip . repeat) list $ tails list
