{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T

parseCard text =
  cardCount
  where
    (cardHeader : cardValues : _) = T.splitOn ": " text
    cardId = (read :: String -> Int) $ (T.unpack . last . T.splitOn " ") cardHeader
    (cardWinNumbers : cardOwnNumbers : _) = numbersToInt <$> T.splitOn " | " cardValues
    numbersToInt text = read . T.unpack <$> filter (/= "") (T.splitOn " " text) :: [Int]
    cardCount = length $ intersect cardWinNumbers cardOwnNumbers

intersect [] = const []
intersect xs = filter (`elem` xs)

getTotalReward cards id = 1 + sum (getTotalReward cards <$> [(id + 1) .. (id + cards !! id)])

main = do
  lines <- T.splitOn "\n" . T.pack <$> readFile "input.txt"
  let cards = parseCard <$> lines
      total = sum $ getTotalReward cards <$> [0 .. length cards - 1]
  print total
