module Main where

import Flow ((|>))
import Lib
import System.Exit
import System.IO
import Text.Parsec

main :: IO ()
main = do
  input <- readFile "input.txt"
  passports <- parsePassports input
  putStrLn $ "The answer is " <> (verifyPassports passports |> countValid |> show) <> "!"

parsePassports :: String -> IO [Passport]
parsePassports input =
  case parse passportListParser "input.txt" input of
    Left error -> die ("could not parse input: " <> show error)
    Right passports -> return passports

verifyPassports :: [Passport] -> [Bool]
verifyPassports = fmap isValidPassport

countValid :: [Bool] -> Int
countValid bools = filter (== True) bools |> length

prompt :: String -> IO Int
prompt text = do
  putStr text
  hFlush stdout
  readInt

readInt :: IO Int
readInt = readLn