module Primes where

import Data.List (genericReplicate)
import qualified Data.Map.Strict as M

-- Given a list of consecutive integers, with leading composites set to 0
-- Will set to zero all composites
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (0:xs) = 0 : sieve xs
sieve (x:xs) = x : sieve (sieveEvery x xs)
  where sieveEvery n = zipWith (*) (markComposites n)
        markComposites n = tail (cycle (0 : genericReplicate (n - 1) 1))

-- Generates an infinite list of prime numbers
primes :: [Integer]
primes = filter (/=0) (sieve [2..])

factorize :: Integer -> M.Map Integer Int
factorize = f primes M.empty
  where f _ m 1 = m
        f [] m _ = m
        f (p:ps) m n =
          case n `divMod` p of
            (d, 0) -> f (p:ps) (M.alter inc p m) d
            (_, _) -> f ps m n

        inc Nothing = Just 1
        inc (Just x) = Just $ x + 1


