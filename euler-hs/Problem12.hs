module Problem12 where

import Data.List (group)
import Problem3 (primeFactors)

factors :: Integer -> [Integer]
factors n = 
  filter ((== 0) . flip mod n)
         [1 .. n]

triang :: Integer -> Integer
triang n = n * (n + 1) `div` 2

triangs :: [Integer]
triangs = map triang [1 ..]

factorCount :: Integer -> Int
factorCount = length . factors

firstFivehundred :: [Integer]
firstFivehundred = dropWhile ((< 500) . factorCount) triangs

main :: IO ()
main = print . head $ firstFivehundred

problem12 :: Integer
problem12 = head $ filter ((> 500) . nDivisors) triangleNumbers
  where nDivisors n = 
          product $
          map ((+ 1) . length)
              (group (primeFactors n))
        triangleNumbers = scanl1 (+) [1 ..]
