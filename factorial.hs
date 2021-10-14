module Factorial where

factNum :: Int -> Int
factNum 0 = 1
factNum x = factNum x * factNum x-1
