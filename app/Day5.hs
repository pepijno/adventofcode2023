{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser
import qualified Data.Map.Strict as M
import Control.Monad (guard)
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Semigroup (sconcat)

data Range = Range {
  source :: Int,
  dest :: Int,
  len :: Int
} deriving (Show, Eq)

data Ranges = Ranges {
  ranges :: [Range]
} deriving (Show, Eq)

combineRanges :: Ranges -> Ranges -> Ranges
combineRanges (Ranges a) (Ranges b) = Ranges $ do
  Range srcA destA lenA <- a
  Range srcB destB lenB <- b
  let newDest = destB + max 0 (destA - srcB)
  let newSrc = srcA + max 0 (srcB - destA)
  let newLen = min (destA + lenA) (srcB + lenB) - max destA srcB
  let newRange = Range newSrc newDest newLen
  guard $ ((>0) . len) newRange
  return newRange

parseRow :: Parser Range
parseRow = do
  dest <- natural
  whiteSpace
  source <- natural
  whiteSpace
  len <- natural
  return $ Range { source = source, dest = dest, len = len }

parseRanges :: Parser Ranges
parseRanges = do
  stringLiteral
  char '\n'
  ranges <- sepBy parseRow (char '\n')
  char '\n'
  return $ Ranges { ranges = ranges }

parseInput :: Parser ([Int], [Ranges])
parseInput = do
  string "seeds: "
  seeds <- sepBy1 natural whiteSpace
  string "\n\n"
  ranges <- sepBy parseRanges (char '\n')
  return (seeds, ranges)

fillRange :: Int -> [Range] -> [Range]
fillRange index [] = [Range { source = index, dest = index, len = (maxBound - index) }]
fillRange index (range:ranges) = before : range : fillRange newIndex ranges
  where
    before = Range { source = index, dest = index, len = (source range - index) }
    newIndex = source range + len range

fillRanges :: Ranges -> Ranges
fillRanges = Ranges . filter ((>0) . len) . fillRange 0 . sortOn source . ranges

doSolve :: [Ranges] -> Int
doSolve = minimum . map dest . ranges . foldl1 combineRanges

solve1 :: [String] -> Int
solve1 xs = doSolve filledRanges
  where
    (seeds, ranges) = unsafeParse parseInput $ unlines xs
    seedRanges = Ranges $ map (\s -> Range s s 1) seeds
    filledRanges = seedRanges:(map fillRanges ranges)

seedRanges :: [Int] -> [Range]
seedRanges [] = []
seedRanges (a:b:xs) = Range { source = a, dest = a, len = b}:(seedRanges xs)

solve2 :: [String] -> Int
solve2 xs = doSolve filledRanges
  where
    (seeds, rs) = unsafeParse parseInput $ unlines xs
    seedRs = Ranges $ seedRanges seeds
    filledRanges = seedRs:(map fillRanges rs)

main :: IO()
main = mainWrapper "day5" solve1 solve2
