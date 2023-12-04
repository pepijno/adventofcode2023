module Main where

import Lib
import Parser
import Data.List
import qualified Data.Map.Strict as M

type Card = (Int, (Int, [Int], [Int]))

parseCard :: Parser Card
parseCard = do
  string "Card "
  many whiteSpace
  cardID <- natural
  string ": "
  haves <- many (many whiteSpace *> natural)
  string " | "
  wins <- many (many whiteSpace *> natural)
  return (cardID, (1, haves, wins))

scoreCard :: Card -> Int
scoreCard (_, (_, haves, wins)) = (\x -> (2^x) `div` 2) $ length $ intersect haves wins

solve1 :: [String] -> Int
solve1 = sum . map (scoreCard . unsafeParse parseCard)

addToID :: M.Map Int (Int, [Int], [Int]) -> Int -> Int -> (Int, [Int], [Int])
addToID cards i toAdd = (amount + toAdd, haves, wins)
  where
    (amount, haves, wins) = cards M.! i

processCard :: M.Map Int (Int, [Int], [Int]) -> Int -> M.Map Int (Int, [Int], [Int])
processCard cards i = foldl (\cs d -> M.insert d (addToID cs d amount) cs) cards nextIDs
  where
    (amount, haves, wins) = cards M.! i
    score = length $ intersect haves wins
    nextIDs = if score == 0 then [] else [(i + 1) .. (i + score)]

first :: (a, b, c) -> a
first (a, _, _) = a

solve2 :: [String] -> Int
solve2 xs = sum $ map (first . snd) $ M.toList $ foldl (processCard) cards [1..(length xs)]
  where
    cards = M.fromList $ map (unsafeParse parseCard) xs

main :: IO()
main = mainWrapper "day4" solve1 solve2
