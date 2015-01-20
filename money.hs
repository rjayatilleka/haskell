{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Money (USD,
              EUR,
              Money(..),
              add,
              Money.subtract) where

import Data.Typeable

-- http://spin.atomicobject.com/2014/12/10/typed-language-tdd-part2/

data BigMac = BigMac deriving (Show)

data USD deriving (Typeable)
data EUR deriving (Typeable)

class (Typeable a) => Currency a
instance Currency USD
instance Currency EUR

data Money c = (Currency c) => Money Int
             deriving (Typeable)

instance Show (Money c) where
    show (Money d) = "Money " ++ show d

add :: Money c -> Money c -> Money c
add (Money a) (Money b) = Money $ a + b

subtract :: Money c -> Money c -> Money c
subtract (Money a) (Money b) = Money $ a - b
