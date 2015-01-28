{-# LANGUAGE GADTs #-}

module CascadingList where

infixr 7 :>>
data CascadingList i o where
  Id :: CascadingList i i
  (:>>) :: (i -> b) -> CascadingList b o -> CascadingList i o

runCascade :: CascadingList i o -> i -> o
runCascade Id i = i
runCascade (f :>> c) i = runCascade c $ f i
