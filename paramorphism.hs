para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f base = h
  where h []       =   base
        h (x:xs)   =   f x xs (h xs)

pairs :: [a] -> [(a, a)]
pairs = para f []
  where f x xs ys = map (\n -> (x,n)) (x:xs) ++ ys
