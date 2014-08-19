import Data.List (genericReplicate)

sieveEvery :: Integer -> [Integer] -> [Integer]
sieveEvery n = zipWith ($) sieveToZero
    where sieveToZero = tail (cycle (const 0 : genericReplicate (n - 1) id))

sieve :: [Integer] -> [Integer]
sieve (0:xs) = 0 : sieve xs
sieve (x:xs) = x : sieve (sieveEvery x xs)
sieve [] = []

primes :: [Integer]
primes = filter (/=0) (sieve [2..])
