module ArmstrongNumbers (armstrong) where

import Data.Char
import Data.List

armstrong :: (Integral a, Show a) => a -> Bool
armstrong x = x == sum (map (^ genericLength (digits x)) (digits x))
  where digits = map (fromIntegral . digitToInt) . show
