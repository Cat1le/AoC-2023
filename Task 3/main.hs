{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor qualified
import Data.Char (digitToInt, isDigit, isSymbol)
import Data.List (elemIndex)
import Data.Text qualified as T

getTable :: IO [[Char]] = do
  string <- readFile "input.txt"
  let text = T.pack string
      textLines = T.splitOn "\n" text
      lines = T.unpack <$> textLines
   in return lines

data TableNumber = TableNumber
  { value :: String,
    posX :: Int,
    posY :: Int
  }
  deriving (Show)

findNumbers :: [[Char]] -> [TableNumber]
findNumbers table = concatMap row $ zip [0 .. length table - 1] table
  where
    row :: (Int, [Char]) -> [TableNumber]
    row (posY, chars) =
      if hasHead
        then (\(posX, value) -> TableNumber {value, posX, posY}) <$> numbers
        else []
      where
        digits = filter (\(_, x) -> isDigit x) $ zip [0 .. length (head table) - 1] chars
        hasHead = not (null digits)
        digitToStr (i, c) = (i, [c])
        numbers = foldl compute [(digitToStr . head) digits] (drop 1 digits)
        compute :: [(Int, [Char])] -> (Int, Char) -> [(Int, [Char])]
        compute acc r@(i, c) =
          if lastI == i - length lastS
            then take (length acc - 1) acc ++ [(lastI, lastS ++ [c])]
            else acc ++ [digitToStr r]
          where
            (lastI, lastS) = last acc

data Gear = Gear {gearPosX :: Int, gearPosY :: Int} deriving (Show)

findGears :: [[Char]] -> [Gear]
findGears table = concatMap row $ zip [0 .. length table - 1] table
  where
    row (y, chars) = (\(x, _) -> mapToGear x y) <$> filter (\(_, x) -> x == '*') (ic chars)
    mapToGear x y = Gear {gearPosX = x, gearPosY = y}
    ic = zip [0 .. length (head table) - 1]

getSurroundingNumbers :: [[Char]] -> [TableNumber] -> Gear -> [TableNumber]
getSurroundingNumbers table numbers Gear {gearPosX, gearPosY} =
  actualNumbers
  where
    positions =
      [(gearPosX - 1, gearPosY), (gearPosX + 1, gearPosY)]
        ++ ((,gearPosY - 1) <$> [gearPosX - 1 .. gearPosX + 1])
        ++ ((,gearPosY + 1) <$> [gearPosX - 1 .. gearPosX + 1])
    digitPositions = fst <$> filter (\(_, x) -> isDigit x) ((\x -> (x, getChar x)) <$> positions)
    digitStarts = getStartOfNumber <$> digitPositions
    getStartOfNumber (0, y) = (0, y)
    getStartOfNumber (x, y) =
      if isDigit $ getChar (x - 1, y)
        then getStartOfNumber (x - 1, y)
        else (x, y)
    getChar (x, y) = table !! y !! x
    actualNumbers = filter (\TableNumber {posX, posY} -> any (\(a, b) -> a == posX && b == posY) digitStarts) numbers

main = do
  table <- getTable
  let numbers = findNumbers table
      gears = findGears table
      actual = filter (\x -> length x == 2) (getSurroundingNumbers table numbers <$> gears)
      actualValues = ((\TableNumber {value} -> (read :: String -> Int) value) <$>) <$> actual
  print . sum $ (\(x : y : _) -> x * y) <$> actualValues
