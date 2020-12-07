module Main where

find2SummingTo :: Int -> [Int] -> [[Int]]
find2SummingTo sum list = [[a, b] | a <- list, b <- list, a +  b == sum]

find3SummingTo :: Int -> [Int] -> [[Int]]
find3SummingTo sum list = [[a, b, c] | a <- list, b <- list, c <- list, a + b + c == sum]

main = do
  numbers <- map read . lines <$> readFile "1.txt"
  print $ map product $ find2SummingTo 2020 numbers
  print $ map product $ find3SummingTo 2020 numbers
