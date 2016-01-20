module Problem16 (digitalSum, main) where
-------------------------------------------
import Data.Char (digitToInt)
-------------------------------------------
digitalSum :: Integer -> Integer
digitalSum = sum . map (fromIntegral . digitToInt) . show

main :: IO ()
main = print . digitalSum $ 2 ^ (1000 :: Integer)
