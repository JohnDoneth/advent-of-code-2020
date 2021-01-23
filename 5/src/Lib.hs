{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( solver,
  )
where

import Control.Monad (liftM2, void)
import Data.Either
import Data.HashMap.Strict
import Data.Hashable
import Debug.Trace
import Flow ((|>))
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

data Choice
  = CLeft
  | CRight

decisions :: [Choice] -> (Int, Int) -> (Int, Int)
decisions choices pair = foldl decide pair choices

decide :: (Int, Int) -> Choice -> (Int, Int)
decide (rangeMin, rangeMax) choice =
  case choice of
    CLeft -> (rangeMin, floor midpoint)
    CRight -> (ceiling midpoint, rangeMax)
  where
    midpoint = ((toRational rangeMax - toRational rangeMin) / 2) + toRational rangeMin

uniqueSeatID :: (Int, Int) -> Int
uniqueSeatID (row, col) = row * 8 + col

choiceParser :: Parser Choice
choiceParser = do
  c <- oneOf ['F', 'B', 'R', 'L']
  case c of
    x
      | x `elem` ['F', 'L'] -> return CLeft
      | x `elem` ['B', 'R'] -> return CRight

choiceListParser :: Parser ([Choice], [Choice])
choiceListParser = do
  row <- count 7 choiceParser
  col <- count 3 choiceParser
  return (row, col)

-- (seatRow row, seatColumn col, uniqueSeatID)
solver :: String -> (Int, Int, Int)
solver input =
  case parse choiceListParser "" input of
    Left error -> (-1, -1, -1)
    Right (row, col) ->
      (sr, sc, uniqueSeatID (sr, sc))
      where
        sr = seatRow row
        sc = seatColumn col


seatRow :: [Choice] -> Int
seatRow choices = decisions choices (0, 127) |> fst

seatColumn :: [Choice] -> Int
seatColumn choices = decisions choices (0, 7) |> fst