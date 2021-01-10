module Lib
  ( solver,
    parseInput,
    parsePolicy,
  )
where

import Control.Monad (liftM2)
import Flow ((|>))
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

data PasswordPolicy = PasswordPolicy
  { -- Password must contain at least this many of `character`.
    minOccurances :: Integer,
    -- Password must contain at most this many of `character`.
    maxOccurances :: Integer,
    -- The character that is being compared.
    character :: Char
  }
  deriving (Show)

data Input = Input
  { policy :: PasswordPolicy,
    password :: String
  }
  deriving (Show)

parseInt :: Parser Integer
parseInt = read <$> many1 digit

parseString :: Parser String
parseString = read <$> many1 lower

parsePolicy :: Parser PasswordPolicy
parsePolicy = do
  [min, max] <- sepBy1 parseInt (char '-')
  _ <- skipMany1 space
  character <- lower

  return
    PasswordPolicy
      { minOccurances = min,
        maxOccurances = max,
        character = character
      }

parseInput :: Parser Input
parseInput = do
  policy <- parsePolicy
  _ <- char ':'
  _ <- skipMany1 space
  password <- many1 lower

  return
    Input
      { policy = policy,
        password = password
      }

-- This works to solve the first part of the puzzle
--
-- https://adventofcode.com/2020/day/2
validateInputStep1 :: Input -> Bool
validateInputStep1
  Input
    { policy =
        PasswordPolicy
          { minOccurances = minOccurances,
            maxOccurances = maxOccurances,
            character = character
          },
      password = password
    } =
    times >= minOccurances && times <= maxOccurances
    where
      times :: Integer
      times = occurances password character

-- Solves part 2 of the puzzle
--
-- https://adventofcode.com/2020/day/2#part2
validateInputStep2 :: Input -> Bool
validateInputStep2
  Input
    { policy =
        PasswordPolicy
          { minOccurances = minOccurances,
            maxOccurances = maxOccurances,
            character = character
          },
      password = password
    } =
    (firstCharacter == character) /= (secondCharacter == character)
    where
      firstCharacter = password !! fromInteger (minOccurances - 1)
      secondCharacter = password !! fromInteger (maxOccurances - 1)

occurances :: String -> Char -> Integer
occurances input c =
  input
    |> filter (== c)
    |> length
    |> fromIntegral

solver :: String -> Bool
solver input =
  case parse parseInput "" input of
    Left failed -> False
    Right result -> validateInputStep2 result
