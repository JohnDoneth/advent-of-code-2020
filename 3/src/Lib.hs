module Lib
  ( solver,
    rowParser,
    treeParser,
    treeMapParser,
    isTreeAt,
    TreeMap (..),
  )
where

import Control.Monad (liftM2, void)
import Flow ((|>))
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Debug.Trace

-- Represents Y X of the tree map
newtype TreeMap = TreeMap [[Bool]]
  deriving (Show, Eq)

width :: TreeMap -> Int
width (TreeMap map) = head map |> length

height :: TreeMap -> Int
height (TreeMap map) = length map

isTreeAt :: TreeMap -> Int -> Int -> Bool
isTreeAt (TreeMap map) x y =
  (map !! ymod) !! xmod
  where
    xmod = x `mod` width (TreeMap map)
    ymod = y `mod` height (TreeMap map)

treeMapParser :: Parser TreeMap
treeMapParser = do
  fmap TreeMap (manyTill rowParser eof)

rowParser :: Parser [Bool]
rowParser =
  manyTill treeParser (choice [fmap (const ()) endOfLine, eof])

treeParser :: Parser Bool
treeParser = do
  fmap (== '#') (oneOf ".#")

solver :: TreeMap -> Int -> Int -> Int
solver treeMap slopeRight slopeDown =
  recurse treeMap slopeRight slopeDown slopeRight slopeDown 0

-- treeMap x y slopeRight slopeDown acc -> Total Trees Hit
recurse :: TreeMap -> Int -> Int -> Int -> Int -> Int -> Int
recurse treeMap x y slopeRight slopeDown acc =
  if y > height treeMap then
    acc
  else
    recurse treeMap (x + slopeRight) (y + slopeDown) slopeRight slopeDown acc2
  where
    acc2 = if isTreeAt treeMap x y then
      acc + 1
    else
      acc