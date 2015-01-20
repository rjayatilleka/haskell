{-# LANGUAGE DeriveDataTypeable #-}

import Money
import Data.Typeable

m :: Money USD
m = Money 4

data YEN deriving (Typeable)

y :: Money YEN
y = Money 5

main = putStrLn $ show m
