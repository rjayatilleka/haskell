module Eight (solutions) where

type Board = [Int]

next :: Board -> [Board]
next b = filter validMove . map (:b) $ [1..8]

validMove :: Board -> Bool
validMove b = validVertical b &&
              validPositive b &&
              validNegative b

validVertical :: Board -> Bool
validVertical [] = True
validVertical (x:xs) = x `notElem` xs

validPositive :: Board -> Bool
validPositive [] = True
validPositive (x:xs) = and . zipWith (/=) [x+1 ..] $ xs

validNegative :: Board -> Bool
validNegative [] = True
validNegative (x:xs) = and . zipWith (/=) [x-1, x-2 ..] $ xs

solutions :: [Board]
solutions = steps !! 8
  where steps = iterate (concatMap next) [[]]
