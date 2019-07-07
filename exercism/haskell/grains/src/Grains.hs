module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n
  | n > 0 && n <= 64 = Just (2^pred n)
  | otherwise = Nothing

total :: Integer
total = sum $ mapMaybe square [1..64]
