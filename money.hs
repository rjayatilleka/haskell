{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable

-- http://spin.atomicobject.com/2014/12/10/typed-language-tdd-part2/

data BigMac = BigMac deriving (Show)

buyBM :: Money USD -> (BigMac, Money USD)
buyBM (Money d) = (BigMac, Money $ d - 1)


class (Typeable a) => Currency a

data USD deriving (Typeable)
data EUR deriving (Typeable)

instance Currency USD
instance Currency EUR

data Money c = (Currency c) => Money Double
             deriving (Typeable)

instance Show (Money c) where
    show (Money d) = "Money " ++ show d

add :: Money c -> Money c -> Money c
add (Money a) (Money b) = Money $ a + b

subtract :: Money c -> Money c -> Money c
subtract (Money a) (Money b) = Money $ a - b
