module Problem52 where

import Data.Char (digitToInt)
import Data.List (sort)

{-
Problem 52
It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
-}
digits :: Int -> [Int]
digits = map (fromIntegral . digitToInt) . show

digs :: Int -> [Int]
digs = sort . digits

allEqual :: (Eq a)
         => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:xs@(y:_)) = x == y && allEqual xs

answer :: Int -- REALLY F*CKIN' SLOW
answer = 
  minimum $
  filter (\x -> allEqual $ map digs [p * x | p <- [1 .. 6]])
         [1 ..]

main :: IO ()
main = print answer
