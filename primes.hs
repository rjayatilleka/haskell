module Primes
 (primes, factorize) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as Q

type SieveQueue = Q.MinPQueue Integer [Integer]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs Q.empty)

sieve' :: [Integer] -> SieveQueue -> [Integer]
sieve' [] _ = []
sieve' (x:xs) table
  | next == x = sieve' xs (adjust x table)
  | otherwise = x : sieve' xs (insertPrime x xs table)
  where (next, _) = Q.findMin table

insertPrime :: Integer -> [Integer] -> SieveQueue -> SieveQueue
insertPrime x xs table = Q.insert (x*x) (map (*x) xs) table

adjust :: Integer -> SieveQueue -> SieveQueue
adjust x table =
  let (ps, table') = Q.spanWithKey (\k _ -> k == x) table
  in foldl' (\table (_, n:ns) -> Q.insert n ns table) table' ps

wheel :: [Integer]
wheel = cycle [2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6,
               2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2,
               4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4,
               2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10]

spin :: [Integer] -> Integer -> [Integer]
spin (x:xs) n = n : spin xs (n + x)

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : sieve (spin wheel 11)

factorize :: Integer -> M.Map Integer Int
factorize = factorize' primes M.empty

factorize' :: [Integer] -> M.Map Integer Int -> Integer -> M.Map Integer Int
factorize' _ m 1 = m
factorize' [] m _ = m
factorize' (p:ps) m n =
  case n `divMod` p of
    (d, 0) -> factorize' (p:ps) (M.alter inc p m) d
    (_, _) -> factorize' ps m n
  where inc Nothing = Just 1
        inc (Just x) = Just $ x + 1
