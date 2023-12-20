{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (isJust)
import Data.Text qualified as T

data GameSubset = GameSubset
  { red :: Maybe Int,
    green :: Maybe Int,
    blue :: Maybe Int
  }
  deriving (Show)

type Game = (Int, [GameSubset])

parseInput :: String -> [Game]
parseInput text =
  parseRow <$> lines
  where
    lines = (T.splitOn "\n" . T.pack) text
    parseRow row =
      (gameId, subsets)
      where
        (gameHead : gameTail : _) = T.splitOn ": " row
        gameId = ((read :: String -> Int) . T.unpack . T.drop 5) gameHead
        subsetsText = T.splitOn "; " gameTail
        subsetsList = T.splitOn ", " <$> subsetsText
        subsets = parseSubset <$> subsetsList
        parseSubset balls =
          GameSubset
            { red = find "red",
              green = find "green",
              blue = find "blue"
            }
          where
            find m =
              mapEntries entries
              where
                entries = filter (T.isSuffixOf m) balls
                mapEntries [] = Nothing
                mapEntries [x] = Just $ (read . T.unpack . head . T.splitOn " ") x

isGamePossible :: Game -> Bool
isGamePossible (_, subsets) = True
  -- all (\GameSubset {red, green, blue} -> getOrZero red <= 12 && getOrZero green <= 13 && getOrZero blue <= 14) subsets
  -- where
  --   getOrZero (Just i) = i
  --   getOrZero Nothing = 0

main = do
  text <- readFile "input.txt"
  let matches = filter isGamePossible $ parseInput text
      getAny :: (GameSubset -> Maybe Int) -> Game -> Int
      getAny fn (_, subsets) = maximum sets
        where
          sets = (\(Just r) -> r) <$> filter isJust (fn <$> subsets)
      getRed = getAny (\GameSubset {red} -> red)
      getGreen = getAny (\GameSubset {green} -> green)
      getBlue = getAny (\GameSubset {blue} -> blue)
      total = sum $ (\x -> getRed x * getGreen x * getBlue x) <$> matches
   in print total
