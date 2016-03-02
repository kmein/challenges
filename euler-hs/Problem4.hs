module Problem4 (digits, isPalindrome, numbers, main) where

import Data.Char (digitToInt)

digits :: Int -> [Int]
digits = map digitToInt . show

isPalindrome :: Int -> Bool
isPalindrome n = reverse (digits n) == digits n

numbers :: [Int]
numbers = 
  [x * y | x <- [999,998 .. 100] :: [Int]
         , y <- [999,998 .. 100] :: [Int]]

main :: IO ()
main = print . maximum . take 10 $ filter isPalindrome numbers
