#!/usr/bin/env runghc

import Data.List (intercalate)

main :: IO ()
main = putStrLn $ intercalate " " $ map show [1..10]
