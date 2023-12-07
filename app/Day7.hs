{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}
module Main where

import Lib
import Parser
import Data.List
import Data.Maybe

parseHand :: Parser (String, Int)
parseHand = do
  cards <- takeN 5 anyChar
  whiteSpace
  bid <- natural
  return (cards, bid)

winnings :: (String -> [Int]) -> [(String, Int)] -> Int
winnings valueMap input = sum [bid * rank | rank <- [1..] | (hand, bid) <- input, then sortOn by valueMap hand]

handToValue :: String -> [Int]
handToValue hand = (handType hand) : (map (\x -> fromJust (elemIndex x "J23456789TQKA")) hand)

handType :: String -> Int
handType m = case sort $ map length $ group $ sort m of
  [5] -> 6
  [1,4] -> 5
  [2,3] -> 4
  [1,1,3] -> 3
  [1,2,2] -> 2
  [1,1,1,2] -> 1
  [1,1,1,1,1] -> 0

solve1 :: [String] -> Int
solve1 = winnings handToValue . map (unsafeParse parseHand)

handToValue2 :: String -> [Int]
handToValue2 hand = maximum [ handType (map replaceJoker hand) : map (\x -> fromJust (elemIndex x "J23456789TQKA")) hand | alt <- "23456789TQKA", let replaceJoker x = if x == 'J' then alt else x ]

solve2 :: [String] -> Int
solve2 = winnings handToValue2 . map (unsafeParse parseHand)

main :: IO()
main = mainWrapper "day7" solve1 solve2
