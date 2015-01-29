module Primes where

import Data.List (foldl')
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

primes :: [Integer]
primes = sieve [2..]
