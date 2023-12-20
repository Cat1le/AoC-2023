{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (isJust, listToMaybe)
import Data.Text qualified as T

getLines = do
  text <- readFile "input.txt"
  (return . T.splitOn "\n" . T.pack) text

getEntries text =
  entryToNumber (head matches, last matches)
  where
    numbers = [(a, (T.pack . show) a, b) | (a, b) <- zip [1 .. 9] numbersAsText]
    numbersAsText = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] :: [T.Text]
    getMatch substr = listToMaybe $ (\(x, _, _) -> x) <$> filter (getMatchPredicate substr) numbers
    getMatchPredicate substr (num, dig, txt) = T.isPrefixOf dig substr || T.isPrefixOf txt substr
    induces = [0 .. (T.length text - 1)]
    subtexts = (`T.drop` text) <$> induces
    matches = (\(Just r) -> r) <$> filter isJust (getMatch <$> subtexts)

entryToNumber (a, b) = a * 10 + b

main = do
  lines <- getLines
  print $ sum (getEntries <$> lines)
