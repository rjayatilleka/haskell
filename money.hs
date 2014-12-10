{-# LANGUAGE MultiParamTypeClasses #-}

-- http://spin.atomicobject.com/2014/12/10/typed-language-tdd-part2/

class Money m where
    money :: (Money m) => Double -> m
    amount :: (Money m) => m -> Double
    add :: (Money m) => m -> m -> m
    add a b = money $ amount a + amount b

newtype Dollar = Dollar Double
                 deriving (Show, Eq)

instance Money Dollar where
    money = Dollar
    amount (Dollar a) = a

newtype Franc = Franc Double
                 deriving (Show, Eq)

instance Money Franc where
    money = Franc
    amount (Franc a) = a

