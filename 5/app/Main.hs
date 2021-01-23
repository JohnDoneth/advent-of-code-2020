module Main where

import Control.Lens (element, (^?))
import Data.List
import Data.Sort
import Debug.Trace
import Flow ((|>))
import Lib
import System.Exit
import System.IO
import Text.Parsec

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  putStrLn (stringToSeats input |> isolateSeatIDs |> highestSeatID |> show)
  return ()
  where
    highestSeatID input = sort input |> reverse |> head

part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  putStrLn (stringToSeats input |> isolateSeatIDs |> sort |> findHoles |> show)
  return ()

stringToSeats :: String -> [(Int, Int, Int)]
stringToSeats input =
  input |> words |> map solver

isolateSeatIDs :: [(Int, Int, Int)] -> [Int]
isolateSeatIDs = map (\(_, _, c) -> c)

findHoles :: [Int] -> [Int]
findHoles input = windows 3 input |> map isAHole |> filter fst |> map snd

isAHole :: [Int] -> (Bool, Int)
isAHole list =
  case (a, b, c) of
    (Just a, Just b, Just c) -> (((a + 1) /= b) || ((c - 1) /= b), b)
    (_, Just b, _) -> (False, b)
    _ -> (False, 0)
  where
    a = list ^? element 0
    b = list ^? element 1
    c = list ^? element 2

-- https://twitter.com/Helkafen/status/701473861351526400
windows n xs = map (take n) (tails xs)
