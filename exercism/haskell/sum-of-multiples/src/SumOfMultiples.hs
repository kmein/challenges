module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (`isMultipleOf` factors) [0..limit-1]
  where 
    x `isMultipleOf` fs = any (`divides` x) fs
    f `divides` x
      | f == 0 = x == 0
      | otherwise = x `mod` f == 0
