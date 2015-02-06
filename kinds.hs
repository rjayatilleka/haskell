{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Kinds where

data Currency = USD
              | EUR
              | YEN
              deriving (Show)

data Money :: Currency -> * where
  Money :: Int -> Money c
  deriving (Show)

add :: Money c -> Money c -> Money c
add (Money a) (Money b) = Money $ a + b
