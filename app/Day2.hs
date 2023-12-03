module Main where

import Lib
import Parser

data Color = Red Int | Blue Int | Green Int deriving (Show, Eq)

data GameTurn = GameTurn [Color] deriving (Show, Eq)

data Game = Game {
    gameID :: Int,
    turns :: [GameTurn]
  } deriving (Show, Eq)

parseColor = choice [
  Red <$> (natural <* string " red"),
  Blue <$> (natural <* string " blue"),
  Green <$> (natural <* string " green")
  ]

parseGameTurn :: Parser GameTurn
parseGameTurn = GameTurn <$> sepBy parseColor (string ", ")

parseGame :: Parser Game
parseGame = do
  string "Game "
  gameID <- natural
  string ": "
  turns <- sepBy parseGameTurn (string "; ")
  return Game { gameID = gameID, turns = turns }

colorValid color = case color of
  Red x -> x <= 12
  Green x -> x <= 13
  Blue x -> x <= 14

gameTurnValid (GameTurn colors) = all colorValid colors

gameValid game = all gameTurnValid $ turns game

-- solve1 :: [String] -> Int
solve1 = sum . map gameID . filter gameValid . map (unsafeParse parseGame)

data Mins = Mins {
    red :: Int,
    green :: Int,
    blue :: Int
  } deriving (Show, Eq)

mulMins Mins { red = r, green = g, blue = b } = r * g * b

updateMinsColor Mins { red = r, green = g, blue = b } color = case color of
  Red x -> Mins { red = max r x, green = g, blue = b }
  Green x -> Mins { red = r, green = max x g, blue = b }
  Blue x -> Mins { red = r, green = g, blue = max x b }

updateMinsTurn :: Mins -> GameTurn -> Mins
updateMinsTurn mins (GameTurn colors) = foldl updateMinsColor mins colors

updateMinsGame mins game = foldl updateMinsTurn mins $ turns game

-- solve2 :: [String] -> Int
solve2 = sum . map mulMins . map (updateMinsGame Mins { red = 0, green = 0, blue = 0 }) . map (unsafeParse parseGame)

main :: IO()
main = mainWrapper "day2" solve1 solve2
