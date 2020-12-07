module Main where

import Data.List (nub, intersect)
import Data.List.Split (splitOn)

anyoneAffirmative :: [[Char]] -> Int
anyoneAffirmative = length . nub . concat

everyoneAffirmative :: [[Char]] -> Int
everyoneAffirmative = length . foldl1 intersect

main = do
  reports <- map lines . splitOn "\n\n" <$> readFile "6.txt"
  print $ sum $ map anyoneAffirmative reports
  print $ sum $ map everyoneAffirmative reports
