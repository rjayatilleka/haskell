#!/usr/bin/env runghc

fizzbuzz :: [(Int, String)] -> Int -> String
fizzbuzz ps i = if null fb then show i else fb
  where fb = fb' ps i
        fb' [] _ = ""
        fb' ((divisor, str):xs) n
          | mod n divisor == 0 = str ++ fb' xs n
          | otherwise = fb' xs n

fizzbuzz2 :: Int -> String
fizzbuzz2 n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 3 == 0 = "Fizz"
  | mod n 5 == 0 = "Buzz"
  | otherwise = show n

fizzbuzz3 :: Int -> String
fizzbuzz3 n = 
  case (n `mod` 3, n `mod` 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _      -> show n

main :: IO ()
main = mapM_ (putStrLn . fizzbuzz3) [1..100]
-- main = mapM_ (putStrLn . fizzbuzz pairs) [1..100]
  -- where pairs = [(3, "Fizz"), (5, "Buzz")]
