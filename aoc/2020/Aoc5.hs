-- https://adventofcode.com/2020/day/5
module Main where

import Data.Maybe (mapMaybe)
import Data.List (sort, (\\))

type Range = (Int, Int)

identified :: Range -> Maybe Int
identified (low, high) = if low == high then Just low else Nothing

split :: Range -> (Range, Range)
split (low, high) =
  let half = low + ((high - low) `div` 2)
  in ((low, half), (half + 1, high))

findRow :: String -> Range -> Maybe Int
findRow codes range =
  let (lower, upper) = split range
  in case codes of
    'B':xs -> findRow xs upper
    'F':xs -> findRow xs lower
    [] | Just result <- identified range -> Just result
    _ -> Nothing

findColumn :: String -> Range -> Maybe Int
findColumn codes range =
  let (lower, upper) = split range
  in case codes of
    'L':xs -> findColumn xs lower
    'R':xs -> findColumn xs upper
    [] | Just result <- identified range -> Just result
    _ -> Nothing

seatId :: String -> Maybe Int
seatId allCodes =
  let (rowCodes, columnCodes) = splitAt 7 allCodes
  in do
    row <- findRow rowCodes (0, 127)
    column <- findColumn columnCodes (0, 7)
    pure $ row * 8 + column

main = do
  passes <- lines <$> readFile "5.txt"
  let seatIds = mapMaybe seatId passes
  print $ maximum seatIds
  print $ [minimum seatIds..maximum seatIds] \\ sort seatIds
