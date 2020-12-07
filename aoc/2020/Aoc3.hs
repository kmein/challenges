module Main where

import Data.Maybe (mapMaybe)

moduloIndex :: Int -> [a] -> a
moduloIndex index xs = xs !! (index `mod` length xs)

keepEveryNth n xs = mapMaybe (\(index, value) -> if index `mod` n == 0 then Just value else Nothing) $ zip [0..] xs

traversedPoints :: Int -> Int -> [[a]] -> [a]
traversedPoints columnJump rowJump rows = zipWith moduloIndex [0, columnJump ..] (keepEveryNth rowJump rows)


countTrees :: Int -> Int -> [String] -> Int
countTrees columnJump rowJump = length . filter (== '#') . traversedPoints columnJump rowJump

main = do
  allPointRows <- lines <$> readFile "3.txt"
  print $ countTrees 3 1 allPointRows

  print $ product
    [ countTrees 1 1 allPointRows
    , countTrees 3 1 allPointRows
    , countTrees 5 1 allPointRows
    , countTrees 7 1 allPointRows
    , countTrees 1 2 allPointRows
    ]
