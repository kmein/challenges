-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
-- (Please note that the palindromic number, in either base, may not include leading zeros.)
module Problem36 where

ap :: (a -> b -> c) -> (a -> b) -> a -> c
ap f g x = f x (g x)

lift2 :: Monad m
      => (a -> b -> c) -> m a -> m b -> m c
lift2 f mx my = mx >>= \x -> my >>= \y -> return (f x y)

binary :: (Integral a,Show a)
       => a -> String
binary 0 = []
binary x = binary (x `div` 2) ++ show (x `mod` 2)

isPalindrome :: Eq a
             => [a] -> Bool
isPalindrome = (==) `ap` reverse

isDecimalPalindrome, isBinaryPalindrome, isDoubleBasePalindrome
  :: (Show a,Integral a)
  => a -> Bool
isDecimalPalindrome = isPalindrome . show

isBinaryPalindrome = isPalindrome . binary

isDoubleBasePalindrome = lift2 (&&) isDecimalPalindrome isBinaryPalindrome

main :: IO ()
main = print $ sum $ filter isDoubleBasePalindrome [1 .. 999999]
