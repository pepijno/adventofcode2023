module Main where

import Lib
import Parser
import Data.List
import qualified Data.Map.Strict as M

parseNode :: Parser (String, (String, String))
parseNode = do
  node <- letters
  string " = ("
  left <- letters
  string ", "
  right <- letters
  string ")"
  return (node, (left, right))

doInstruction ::M.Map String (String, String) -> String -> Char -> String
doInstruction nodes node i
  | i == 'L' = left
  | i == 'R' = right
  where
    (left, right) = nodes M.! node

walkUntil :: (String -> Bool) -> [Char] -> M.Map String (String, String) -> String -> Int
walkUntil until instructions nodes startNode = length $ takeWhile (not . until) $ scanl (doInstruction nodes) startNode instructions

solve1 :: [String] -> Int
solve1 xs = walkUntil (== "ZZZ") instructions nodes "AAA"
  where
    instructions = cycle $ head xs
    nodes = M.fromList $ map (unsafeParse parseNode) $ drop 2 xs

solve2 :: [String] -> Int
solve2 xs = foldl1 lcm $ map (\n -> walkUntil ((=='Z') . last) instructions nodes n) $ filter ((=='A') . last) $ M.keys nodes
  where
    instructions = cycle $ head xs
    nodes = M.fromList $ map (unsafeParse parseNode) $ drop 2 xs

main :: IO()
main = mainWrapper "day8" solve1 solve2
