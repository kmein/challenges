module Problem20 (factorial, digitalSum, main) where

--------------------------------
import Data.Char (digitToInt)

--------------------------------
factorial :: Integer -> Integer
factorial = product . enumFromTo 1

digitalSum :: Integer -> Integer
digitalSum = sum . map (fromIntegral . digitToInt) . show

main :: IO ()
main = print . digitalSum . factorial $ 100
