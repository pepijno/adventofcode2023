module Main where

import Lib
import Data.List
import Parser

parseInput :: Parser [(Int, Int)]
parseInput = do
  string "Time:"
  many whiteSpace
  times <- many (many whiteSpace *> natural)
  string "\nDistance:"
  many whiteSpace
  distances <- many (many whiteSpace *> natural)
  return $ zip times distances

countWins :: (Int, Int) -> Int
countWins (time, dist) = 1 + time - (2 * index)
  where
    (Just index) = find (\x -> (time - x) * x > dist) [1..time]

solve1 :: [String] -> Int
solve1 = product . map countWins . unsafeParse parseInput . unlines

solve2 :: [String] -> Int
solve2 = countWins . head . unsafeParse parseInput . filter (/= ' ') . unlines

main :: IO()
main = mainWrapper "day6" solve1 solve2
