module Main where

import Data.List
import Data.Char

import Lib
import Grid
import qualified Data.Map.Strict as M
import Data.Char

data Number = Number {
    pos :: [Coord],
    val :: Int
  } deriving (Show, Eq)

parseNumbersLine :: Int -> Grid Char -> [Number] -> Int -> [Number]
parseNumbersLine y grid nums x
  | x > 139 = reverse nums
  | isDigit val && isDigit nextVal && isDigit nextNextVal = parseNumbersLine y grid ((Number { pos = [point, (y, x + 1), (y, x + 2)], val = 100 * (digitToInt val) + 10 * (digitToInt nextVal) + digitToInt nextNextVal}):nums) (x + 4)
  | isDigit val && isDigit nextVal = parseNumbersLine y grid ((Number { pos = [point, (y, x + 1)], val = 10 * (digitToInt val) + digitToInt nextVal}):nums) (x + 3)
  | isDigit val = parseNumbersLine y grid ((Number { pos = [point], val = digitToInt val}):nums) (x + 2)
  | otherwise = parseNumbersLine y grid nums (x + 1)
  where
    point = (y, x)
    val = grid M.! point
    nextVal = grid M.! (y, x + 1)
    nextNextVal = grid M.! (y, x + 2)

isNumberAdjacent :: Grid Char -> Number -> Bool
isNumberAdjacent grid num = any (\c -> c /= '.' && not (isDigit c)) $ map ((M.!) grid) $ concatMap (neighbours dir8 grid) $ pos num

solve1 :: [String] -> Int
solve1 xs = sum $ sort $ map val $ filter (isNumberAdjacent grid) $ concatMap (\y -> parseNumbersLine y grid [] 0) [0..139]
  where
    grid = parseGrid id xs

solve2 :: [String] -> Int
solve2 xs = sum $ map (product . map val) $ filter ((==2) . length) $ map (\star -> filter (\n -> not $ null $ intersect (neighbours dir8 grid star) $ pos n) numbers) stars
  where
    grid = parseGrid id xs
    stars = M.keys $ M.filter (== '*') grid
    numbers = concatMap (\y -> parseNumbersLine y grid [] 0) [0..139]

main :: IO()
main = mainWrapper "day3" solve1 solve2
