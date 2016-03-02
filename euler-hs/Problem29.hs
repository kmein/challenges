module Problem29 where

------------------------
import Data.List (nub, sort)

------------------------
main :: IO ()
main = 
  print . length . nub . sort $
  [a ^ b | a <- [2 .. 100] :: [Int]
         , b <- [2 .. 100] :: [Int]]
