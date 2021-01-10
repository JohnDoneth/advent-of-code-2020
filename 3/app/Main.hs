module Main where

import Flow ((|>))
import Lib
import System.IO
import Text.Parsec
import System.Exit

main :: IO ()
main = do
  input <- readFile "input.txt"
  treeMap <- parseTreeMap input
  slopeLeft <- prompt "Enter slope right: "
  slopeDown <- prompt "Enter slope down: "
  putStrLn $ "The answer is " <> show (solver treeMap slopeLeft slopeDown) <> "!"

parseTreeMap :: String -> IO TreeMap
parseTreeMap input =
  case parse treeMapParser "input.txt" input of
    Left error -> die ("could not parse input: " <> show error)
    Right treeMap -> return treeMap

prompt :: String -> IO Int
prompt text = do
  putStr text
  hFlush stdout
  readInt

readInt :: IO Int
readInt = readLn