import Data.List (sort)
import qualified Data.Map as Map

adjacentDifferences :: [Int] -> [Int]
adjacentDifferences xs =
  let xs' = sort (0:xs) -- "The charging outlet has an effective rating of 0 jolts"
   in zipWith subtract xs' (tail xs') ++ [3] -- "your device's built-in adapter is always 3 higher than the highest adapter"

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

main = do
  numbers <- map read . lines <$> readFile "10.txt"
  let differences = adjacentDifferences numbers
  print $ count 3 differences * count 1 differences
