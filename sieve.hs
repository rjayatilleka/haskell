import Data.List (genericReplicate)

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
