module Prim where

import Control.Monad.State.Strict
import Data.MemoTrie (memo)
import Data.Numbers.Primes (primeFactors)
import qualified Data.Vector.Unboxed as V

unDigits :: Integral a => [a] -> a
unDigits = foldl (\a b -> a * 10 + b) 0
{-# SPECIALIZE unDigits :: [Word] -> Word #-}
{-# INLINE unDigits #-}

digits :: Integral a => a -> [a]
digits = ds []
  where
    ds acc 0 = acc
    ds acc x = ds (d : acc) r
      where (r, d) = quotRem x 10
{-# SPECIALIZE digits :: Word -> [Word] #-}

score :: [Word] -> Word
score = memo $ last . primeFactors . unDigits . concatMap digits

sans :: (V.Unbox a) => Int -> V.Vector a -> V.Vector a
sans i xs = ys V.++ V.tail zs
  where (ys, zs) = V.splitAt i xs
{-# SPECIALIZE sans :: Int -> V.Vector Word -> V.Vector Word #-}

pickIndices :: [Int] -> V.Vector Word -> Word
pickIndices = (sum .) . evalState . mapM pickIndex
  where
    pickIndex i = gets (\xs -> score $ map (V.unsafeIndex xs) is) <* modify (sans i)
      where is = [pred i, i, succ i]

indices :: Int -> [[Int]]
indices len
  | len <= 2 = []
  | otherwise = mapM (enumFromTo 1) [len - 2,len - 3 .. 1]

scores :: V.Vector Word -> [([Int], Word)]
scores xs = map (\is -> (is, pickIndices is xs)) $ indices $ V.length xs
