import Control.Monad

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
    a <- x
    liftM (a:) $ sequence xs

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = do
    b <- f x
    liftM (b:) $ mapM' f xs

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do
    t <- p x
    if t then liftM (x:) $ filterM' p xs
         else              filterM' p xs
