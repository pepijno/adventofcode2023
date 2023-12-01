{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser
import Data.Char
import Data.List
-- import Data.Text(pack, unpack, replace)

toNumber :: String -> Int
toNumber x = read $ [head digits] ++ [last digits]
  where
    digits = filter isDigit x

solve1 :: [String] -> Int
solve1 = sum . map toNumber

parseDigits :: String -> String
parseDigits x = foldr (\(x, y) acc -> replace x y acc) x [ ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]
 
replace :: String -> String -> String -> String
replace _ _ [] = []
replace _ _ [x] = [x]
replace original new whole@(x : y : xs) =
  if original `isPrefixOf` whole
    then replace original new (x : new <> xs)
    else x : replace original new (y : xs)

solve2 :: [String] -> Int
solve2 = sum . map (toNumber . parseDigits)

main :: IO()
main = mainWrapper "day1" solve1 solve2
