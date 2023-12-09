module Main where

import Lib

readAsList :: [String] -> [[Int]]
readAsList = map (map read . words)

next :: [Int] -> Int
next = sum . map last . takeWhile (any (0 /=)) . iterate differences

differences :: [Int] -> [Int]
differences xs = zipWith (flip (-)) xs (tail xs)

solve1 :: [String] -> Int
solve1 = sum . map next . readAsList

solve2 :: [String] -> Int
solve2 = sum . map (next . reverse) . readAsList

main :: IO()
main = mainWrapper "day9" solve1 solve2
