{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( isValidPassport,
    isValidPassportS,
    parsePassport,
    passportListParser,
    singleNewLine,
    passportParser,
    twoNewLines,
    fieldValueParser,
    sValidateHeight,
    sValidateBirthYear,
    sValidateHairColor,
    sValidateEyeColor,
    sValidatePassportID,
    Passport,
  )
where

import Control.Monad (liftM2, void)
import Data.HashMap.Strict
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

data HairColor
  = Amber
  | Blue
  | Brown
  | Gray
  | Green
  | Hazel
  | Other
  deriving (Show, Eq)

newtype HexColor = HexColor String deriving (Show, Eq)

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

newtype Passport = Passport (HashMap PassportFieldTag String)
  deriving (Show)

-- hexParser :: Parser Color
-- hexParser = do
--   _ <- char '#'
--   fmap Hex (many1 hexDigit)

-- colorParser :: Parser Color
-- colorParser =
--   choice
--     [ fmap Text (many1 letter),
--       hexParser
--     ]

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

fieldParser :: Parser (PassportFieldTag, String)
fieldParser = do
  tag <- fieldTagParser
  _ <- char ':'
  value <- fieldValueParser
  return (tag, value)

fieldValueParser :: Parser String
fieldValueParser = many1 (try (choice [alphaNum, char '#']))

passportListParser :: Parser [Passport]
passportListParser = endBy passportParser singleNewLine

twoNewLines :: Parser [Char]
twoNewLines = do
  count 2 endOfLine

singleNewLine :: Parser Char
singleNewLine = do
  e <- try endOfLine
  _ <- notFollowedBy (try endOfLine)
  return e

passportParser :: Parser Passport
passportParser = do
  tags <-
    endBy1
      fieldParser
      (choice [space, singleNewLine])
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
          then member field hashSet
          else False

parsePassport input = parse passportListParser "" input

isValidPassportS :: String -> Bool
isValidPassportS input =
  case parse passportParser "" input of
    Left error -> False
    Right passport -> isValidPassport passport

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

validatePassportBirthYear :: Passport -> Bool
validatePassportBirthYear (Passport map) =
  case Data.HashMap.Strict.lookup BirthYear map of
    Just a ->
      case parse yearParser "" a of
        Left failed -> False
        Right year -> True
    Nothing -> False

stringToYear :: String -> Maybe Int
stringToYear input =
  case parse yearParser "" input of
    Left failed -> Nothing
    Right year -> Just year

yearParser :: Parser Int
yearParser = fmap read (count 4 digit)

validateBirthYear :: Int -> Bool
validateBirthYear v = v >= 1920 && v <= 2002

sValidateBirthYear :: String -> Bool
sValidateBirthYear input = False

validateIssueYear :: Int -> Bool
validateIssueYear v = v >= 2010 && v <= 2020

validateExpirationYear :: Int -> Bool
validateExpirationYear v = v >= 2020 && v <= 2030

sValidateHeight :: String -> Bool
sValidateHeight input =
  case parse heightParser "" input of
    Left error -> False
    Right height -> validateHeight height

heightParser :: Parser Height
heightParser = do
  value <- manyTill digit (lookAhead (oneOf ['i', 'c']))
  chars <- choice [string "in", string "cm"]
  return
    ( case chars of
        "in" -> Inches (read value)
        "cm" -> Centimeters (read value)
    )

validateHeight :: Height -> Bool
validateHeight (Centimeters cm) = cm >= 150 && cm <= 193
validateHeight (Inches inches) = inches >= 59 && inches <= 76

sValidateEyeColor :: String -> Bool
sValidateEyeColor color =
  color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

sValidateHairColor :: String -> Bool
sValidateHairColor input = False

sValidatePassportID :: String -> Bool
sValidatePassportID input = False

sValidateCountryID :: String -> Bool
sValidateCountryID input = False