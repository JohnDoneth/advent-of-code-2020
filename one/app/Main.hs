module Main where

import Lib

main :: IO ()
main = do
  integers <- readInts "input.txt"
  putStrLn $ "The answer is " <> show (solver integers) <> "!"

readInts :: FilePath -> IO [Integer]
readInts path = do
  lines <- readLines path
  return $ fmap read lines

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile