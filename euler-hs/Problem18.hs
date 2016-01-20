module Problem18 where

import Utils.List
import Utils.Function.Combinator
import Utils.Memo

maxValue :: (Int, Int) -> [[Int]] -> Int
maxValue (i, j) pyr 
    | i == length pyr = 0
    | otherwise       = pyr !!! (i, j) + maxValue (i+1, j) pyr `max` maxValue (i+1, j+1) pyr

getPyramid :: (Read r) => FilePath -> IO [[r]]
getPyramid = (map (map read . words) . lines) *. readFile

main :: IO ()
main = print . memo (maxValue (0,0)) =<< getPyramid "67.txt"
