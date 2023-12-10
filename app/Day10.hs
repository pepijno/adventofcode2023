module Main where

import Lib
import Grid
import qualified Data.Map.Strict as M

walkGrid :: Grid Char -> Grid Int -> [(Coord, Int)] -> Grid Int
walkGrid _ had [] = had
walkGrid grid had ((pos@(y,x), val):xs)
  | M.member pos had = walkGrid grid had xs
  | c == '-' = walkGrid grid newHad (xs ++ [((y, x - 1), val + 1), ((y, x + 1), val + 1)])
  | c == '|' = walkGrid grid newHad (xs ++ [((y - 1, x), val + 1), ((y + 1, x), val + 1)])
  | c == 'J' = walkGrid grid newHad (xs ++ [((y, x - 1), val + 1), ((y - 1, x), val + 1)])
  | c == 'L' = walkGrid grid newHad (xs ++ [((y, x + 1), val + 1), ((y - 1, x), val + 1)])
  | c == 'F' = walkGrid grid newHad (xs ++ [((y, x + 1), val + 1), ((y + 1, x), val + 1)])
  | c == '7' = walkGrid grid newHad (xs ++ [((y, x - 1), val + 1), ((y + 1, x), val + 1)])
  where
    c = grid M.! pos
    newHad = M.insert pos val had

solve1 :: [String] -> Int
solve1 xs = maximum $ M.elems $ walkGrid grid (M.singleton start 0) [((113, 18), 1), ((112, 19), 1)]
  where
    grid = parseGrid id xs
    start = (112, 18)

data ComeFrom = Up | Down | None deriving (Show, Eq)

findEnclosed :: Grid Char -> Grid Int -> Bool -> ComeFrom -> [Coord] -> [Coord] -> [Coord]
findEnclosed _ _ _ _ i [] = i
findEnclosed grid had started comeFrom i (x:xs)
  | not (M.member x had) && started = findEnclosed grid had started comeFrom (x:i) xs
  | not (M.member x had) && not started = findEnclosed grid had started comeFrom i xs
  | c == '|' = findEnclosed grid had (not started) None i xs
  | c == 'L' = findEnclosed grid had started Up i xs
  | c == 'F' || c == 'S' = findEnclosed grid had started Down i xs
  | c == '-' = findEnclosed grid had started comeFrom i xs
  | c == 'J' = if comeFrom == Down then findEnclosed grid had (not started) None i xs else findEnclosed grid had started None i xs
  | c == '7' = if comeFrom == Up then findEnclosed grid had (not started) None i xs else findEnclosed grid had started None i xs
  | otherwise = findEnclosed grid had started comeFrom i xs
  where
    c = grid M.! x

findAllEnclosed :: Grid Char -> Grid Int -> Int
findAllEnclosed grid had = sum $ map (\y -> length $ findEnclosed grid had False None [] $ map (\x -> (y, x)) [0..139]) [0..139]

solve2 :: [String] -> Int
solve2 xs = findAllEnclosed grid $ walkGrid grid (M.singleton start 0) [((113, 18), 1), ((112, 19), 1)]
  where
    grid = parseGrid id xs
    start = (112, 18)

main :: IO()
main = mainWrapper "day10" solve1 solve2
