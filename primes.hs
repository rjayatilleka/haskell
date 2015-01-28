module Primes where

import qualified Data.PQueue.Prio.Min as Q

type SieveQueue = Q.MinPQueue Integer [Integer]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs Q.empty)

sieve' :: [Integer] -> SieveQueue -> [Integer]
sieve' [] _ = []
sieve' (x:xs) table
    | nextComposite == x = sieve' xs (adjust table)
    | otherwise = x : sieve' xs (insertPrime x xs table)
    where (nextComposite, _) = Q.findMin table

          adjust :: SieveQueue -> SieveQueue
          adjust table
            | n <= x = adjust . Q.insert n' ns . Q.deleteMin $ table
            | otherwise = table
            where (n, n':ns) = Q.findMin table

insertPrime :: Integer -> [Integer] -> SieveQueue -> SieveQueue
insertPrime x xs table = Q.insert (x*x) (map (*x) xs) table
