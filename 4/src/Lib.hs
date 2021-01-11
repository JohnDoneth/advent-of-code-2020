{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib
  ( isValidPassport,
    isValidPassportS,
    parsePassport,
    passportListParser,
    Passport,
  )
where

import Control.Monad (liftM2, void)
import Data.HashSet
import Data.Hashable
import Debug.Trace
import Flow ((|>))
import GHC.Generics (Generic)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

data Height
  = Centimeters Int
  | Inches Int
  deriving (Show, Eq)

data Color
  = Text String
  | Hex String
  deriving (Show, Eq)

-- data Passport = PassportField
--   { birthYear :: Maybe Int,
--     issueYear :: Maybe Int,
--     expirationYear :: Maybe Int,
--     height :: Maybe Height,
--     hairColor :: Maybe Color,
--     eyeColor :: Maybe Color,
--     passportID :: Maybe String,
--     countryID :: Maybe String
--   }
--   deriving (Show, Eq)

newtype Passport = Passport (HashSet PassportFieldTag)
  deriving (Show)

hexParser :: Parser Color
hexParser = do
  _ <- char '#'
  fmap Hex (many1 hexDigit)

colorParser :: Parser Color
colorParser =
  choice
    [ fmap Text (many1 letter),
      hexParser
    ]

data PassportFieldTag
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

fieldTagParser :: Parser PassportFieldTag
fieldTagParser =
  try (const BirthYear <$> string "byr")
    <|> try (const IssueYear <$> string "iyr")
    <|> try (const ExpirationYear <$> string "eyr")
    <|> try (const Height <$> string "hgt")
    <|> try (const HairColor <$> string "hcl")
    <|> try (const EyeColor <$> string "ecl")
    <|> try (const PassportID <$> string "pid")
    <|> try (const CountryID <$> string "cid")

fieldParser :: Parser PassportFieldTag
fieldParser = do
  tag <- fieldTagParser
  _ <- char ':'
  value <- fieldValueParser
  return tag

fieldValueParser :: Parser ()
fieldValueParser = do
  _ <- many1 (try (choice [alphaNum, char '#']))
  return ()

passportListParser :: Parser [Passport]
passportListParser = endBy passportParser twoNewLines

twoNewLines :: Parser Char
twoNewLines = do
  _ <- endOfLine
  e <- endOfLine
  return e

singleNewLine :: Parser Char
singleNewLine = do
  e <- endOfLine
  _ <- notFollowedBy endOfLine
  return e

passportParser :: Parser Passport
passportParser = do
  tags <- sepBy1 fieldParser (choice [space, try singleNewLine])
  tags
    |> fromList
    |> Passport
    |> return

isValidPassport :: Passport -> Bool
isValidPassport (Passport hashSet) =
  foldl func True requiredFields
  where
    func =
      \a field ->
        if a
          then member field (trace (show hashSet) hashSet)
          else False

parsePassport input = parse passportListParser "" input

isValidPassportS :: String -> Bool
isValidPassportS input =
  case parse passportParser "" input of
    Left error -> False
    Right passport -> isValidPassport (trace (show passport) passport)

requiredFields :: [PassportFieldTag]
requiredFields =
  [ BirthYear,
    IssueYear,
    ExpirationYear,
    Height,
    HairColor,
    EyeColor,
    PassportID
  ]