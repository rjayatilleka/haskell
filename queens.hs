module Queens (solve) where

type Board = [Int]

next :: Int -> Board -> [Board]
next n b = filter validMove . map (:b) $ [1..n]

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

solve :: Int -> [Board]
solve n = steps !! n
  where steps = (iterate . concatMap . next $ n) [[]]
