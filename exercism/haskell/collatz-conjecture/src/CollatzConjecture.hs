module CollatzConjecture (collatz) where

import Data.List (genericLength)

step :: Integer -> Integer
step n
  | even n = n `div` 2
  | odd n = 3 * n + 1
  | otherwise = undefined

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just . genericLength . takeWhile (/= 1) $ iterate step n
