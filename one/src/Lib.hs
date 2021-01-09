module Lib
  ( solver,
  )
where

import Control.Monad (liftM2)
import Flow ((|>))

-- For a given list, returns the product of the pair that sums to 2020.
solver :: [Integer] -> Integer
solver input =
  input
    |> pairs
    |> find2020Pair
    |> multiplyPair

-- Generates a list of all pairs from a list
pairs :: [Integer] -> [(Integer, Integer)]
pairs input = liftM2 (,) input input

multiplyPair :: (Integer, Integer) -> Integer
multiplyPair (a, b) = a * b

find2020Pair :: [(Integer, Integer)] -> (Integer, Integer)
find2020Pair list =
  list
    -- Only look at unique pairs
    |> filter (uncurry (/=))
    -- Filter out pairs that do not sum to 2020
    |> filter (\(a, b) -> a + b == 2020)
    -- Return the first result
    |> head