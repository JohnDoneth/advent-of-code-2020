module Main where

import Flow ((|>))
import Lib

main :: IO ()
main = do
  lines <- readLines "input.txt"
  putStrLn $ "The answer is " <> (validateMany lines |> countSuccesses |> show) <> "!"

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

validateMany :: [String] -> [Bool]
validateMany = fmap solver

countSuccesses :: [Bool] -> Integer
countSuccesses input =
  input
    |> filter (== True)
    |> length
    |> fromIntegral