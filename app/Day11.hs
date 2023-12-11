module Main where

import Lib
import Data.List
import Grid
import qualified Data.Map.Strict as M

emptyRows :: Int -> [String] -> [Int]
emptyRows _ [] = []
emptyRows i (x:xs)
  | all (=='.') x = i:(emptyRows (i + 1) xs)
  | otherwise = emptyRows (i + 1) xs

dist :: [Int] -> [Int] -> Int -> Coord -> Coord -> Int
dist emptyCs emptyRs factor a@(y1, x1) b@(y2, x2) = manhattan a b + (factor - 1) * (rs + cs)
  where
    rs = length $ filter (\y -> (y1 < y && y < y2) || (y2 < y && y < y1)) emptyRs
    cs = length $ filter (\x -> (x1 < x && x < x2) || (x2 < x && x < x1)) emptyCs


solve1 :: [String] -> Int
solve1 xs = (`div` 2) $ sum $ map (uncurry (dist emptyCols emptyRs 2)) [(a, b) | a <- galaxies, b <- galaxies, a /= b]
  where
    expanded = parseGrid id xs
    galaxies = M.keys $ M.filter (=='#') expanded
    emptyRs = emptyRows 0 xs
    emptyCols = emptyRows 0 $ transpose xs

solve2 :: [String] -> Int
solve2 xs = (`div` 2) $ sum $ map (uncurry (dist emptyCols emptyRs 1000000)) [(a, b) | a <- galaxies, b <- galaxies, a /= b]
  where
    expanded = parseGrid id xs
    galaxies = M.keys $ M.filter (=='#') expanded
    emptyRs = emptyRows 0 xs
    emptyCols = emptyRows 0 $ transpose xs

main :: IO()
main = mainWrapper "day11" solve1 solve2
