module Problem25 where

import Data.Bits (bitSize, testBit)
import Data.Char (digitToInt)
import Data.List (elemIndex, foldl')
import Data.Maybe (fromJust)

(|>) :: t1 -> (t1 -> t) -> t
x |> f = f x

fib :: Int -> Integer
fib n = 
  snd . foldl' fib' (1,0) . dropWhile not $
  [testBit n k | k <- 
                  let s = bitSize n
                  in [s - 1,s - 2 .. 0]]
  where fib' (f,g) p
          | p = (f * (f + 2 * g),ss)
          | otherwise = (ss,g * (2 * f - g))
          where ss = f * f + g * g

digits :: Integer -> [Int]
digits = map digitToInt . show

main :: IO ()
main = 
  fibs |> dropWhile ((< 1000) . numOfDigits) |> head |> flip elemIndex fibs |>
  fromJust |>
  (+ 1) |>
  print
  where numOfDigits = length . digits
        fibs = map fib [1 ..]
