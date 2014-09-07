fizzbuzz :: [(Int, String)] -> Int -> String
fizzbuzz ps i = if null fb then show i else fb
  where fb = fb' ps i
        fb' [] _ = ""
        fb' ((divisor, str):xs) n
          | mod n divisor == 0 = str ++ fb' xs n
          | otherwise = fb' xs n

main :: IO ()
main = mapM_ putStrLn results
  where results = map (fizzbuzz pairs) [1..100]
        pairs = [(3, "Fizz"), (5, "Buzz")]
