module Main where

import Lib
import Parser
import qualified Data.Map.Strict as M

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " *> (sepBy natural whiteSpace)

parseRow :: Parser (Int, Int, Int)
parseRow = do
  dest <- natural
  whiteSpace
  source <- natural
  whiteSpace
  len <- natural
  return (source, dest, len)

parseMap :: Parser ((String, String), [(Int, Int, Int)])
parseMap = do
  optional (string "\n")
  from <- letters
  string "-to-"
  to <- letters
  string " map:\n"
  rows <- sepBy parseRow (string "\n")
  string "\n"
  return ((from, to), rows)

mapNumber num [] = num
mapNumber num ((source, dest, len):xs)
  | num < source + len && num >= source = dest + (num - source)
  | otherwise = mapNumber num xs

processNumber maps num key = mapNumber num $ maps M.! key

-- solve1 :: [String] -> Int
solve1 xs = minimum $ map (\seed -> foldl (processNumber maps) seed [("seed", "soil"), ("soil", "fertilizer"), ("fertilizer", "water"), ("water", "light"), ("light", "temperature"), ("temperature", "humidity"), ("humidity", "location")]) seeds
  where
    seeds = unsafeParse parseSeeds $ head xs
    maps = M.fromList $ unsafeParse (many parseMap) $ unlines $ drop 2 xs

mapSeeds [] = []
mapSeeds (a:b:xs) = [a..(a + b)] ++ mapSeeds xs

-- solve2 :: [String] -> Int
-- solve2 xs = minimum $ map (\seed -> foldl (processNumber maps) seed [("seed", "soil"), ("soil", "fertilizer"), ("fertilizer", "water"), ("water", "light"), ("light", "temperature"), ("temperature", "humidity"), ("humidity", "location")]) seeds
solve2 xs = minimum $ map (\seed -> foldl (processNumber maps) seed [("seed", "soil"), ("soil", "fertilizer"), ("fertilizer", "water"), ("water", "light"), ("light", "temperature"), ("temperature", "humidity"), ("humidity", "location")]) seeds
  where
    seeds = mapSeeds $ unsafeParse parseSeeds $ head xs
    maps = M.fromList $ unsafeParse (many parseMap) $ unlines $ drop 2 xs

main :: IO()
main = mainWrapper "day5" solve1 solve2
