{-
fizzbuzz :: Int -> String
fizzbuzz n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 5 == 0 = "Buzz"
  | mod n 3 == 0 = "Fizz"
  | otherwise = show n
-}

fizzbuzz' :: [(Int, String)] -> Int -> String
fizzbuzz' [] _ = ""
fizzbuzz' ((divisor, str):xs) n
  | mod n divisor == 0 = str ++ fizzbuzz' xs n
  | otherwise = fizzbuzz' xs n

fizzbuzz :: [(Int, String)] -> Int -> String
fizzbuzz xs n = if str == ""
                then show n
                else str
  where str = fizzbuzz' xs n

main :: IO ()
main = mapM_ putStrLn results
--  where results = map fizzbuzz [1..100]
  where results = map (fizzbuzz pairs) [1..100]
        pairs = [(3, "Fizz"), (5, "Buzz")]
