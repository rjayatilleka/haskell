{-# LANGUAGE TupleSections #-}

module Sudoku where

import qualified Data.Vector as V
import Data.Maybe
import Data.List (foldl')

type Vector = V.Vector
type Coord = (Int, Int)
type Board = Vector (Vector Int)

solve :: Board -> [Board]
solve b = foldl' next [b] (gaps b)

gaps :: Board -> [Coord]
gaps b = concat
       . V.toList
       . V.imap (\i -> map (i,))
       . V.map (catMaybes . V.toList . V.imap zeroIndex)
       $ b
  where zeroIndex i 0 = Just i
        zeroIndex _ _ = Nothing

next :: [Board] -> Coord -> [Board]
next bs c = filter (validAt c) $ concatMap (movesAt c) bs

movesAt :: Coord -> Board -> [Board]
movesAt c b = map (update c b) [1..9]

validAt :: Coord -> Board -> Bool
validAt c b = all (validate (get c b) . ($ b) . ($ c)) [row, col, box]

update :: Coord -> Board -> Int -> Board
update (x, y) b i = (b V.//)
                  . (:[])
                  . (x,)
                  . (V.// [(y, i)])
                  . (V.! x)
                  $ b

validate :: Int -> Vector Int -> Bool
validate i = (== 1) . V.length . V.elemIndices i

get :: Coord -> Board -> Int
get (x, y) b = (b V.! x) V.! y

row :: Coord -> Board -> Vector Int
row (x, _) = (V.! x)

col :: Coord -> Board -> Vector Int
col (_, y) = V.map (V.! y)

box :: Coord -> Board -> Vector Int
box = box_ . corner
  where c i = (i `div` 3) * 3
        corner (x, y) = (c x, c y)
        box_ (x, y) = V.concatMap (V.slice y 3) . V.slice x 3



--------------------------------------------------------------------------------
----- STUFF --------------------------------------------------------------------

board :: Board
board = readBoard example

example2 :: String
example2 = "000003017\n\
           \015009008\n\
           \060000000\n\
           \100007000\n\
           \009000200\n\
           \000500004\n\
           \000000020\n\
           \500600340\n\
           \340200000"

example :: String
example = "003020600\n\
          \900305001\n\
          \001806400\n\
          \008102900\n\
          \700000008\n\
          \006708200\n\
          \002609500\n\
          \800203009\n\
          \005010300"

readBoard :: String -> Board
readBoard s = let rows = lines s
                  digits = map (map (read . (:[]))) rows
              in V.fromList . map V.fromList $ digits
