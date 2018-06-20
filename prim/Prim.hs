{-# LANGUAGE Strict, ViewPatterns #-}
module Prim where

import Data.List (maximumBy)
import Data.MemoTrie (memo)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import qualified Data.Vector as V

unDigits :: Integral a => [a] -> a
unDigits = foldl (\a b -> a * 10 + b) 0
{-# SPECIALIZE unDigits :: [Word] -> Word #-}
{-# INLINE unDigits #-}

digits :: Integral a => a -> [a]
digits = flip ds []
  where
    ds 0 acc = acc
    ds x acc = ds r (d : acc)
      where (r, d) = quotRem x 10
{-# SPECIALIZE digits :: Word -> [Word] #-}

score :: [Word] -> Word
score = memo (last . primeFactors) . unDigits . concatMap digits

sans :: Int -> V.Vector a -> V.Vector a
sans i xs = ys V.++ V.tail zs
  where (ys, zs) = V.splitAt i xs

pickIndices :: [Int] -> V.Vector Word -> Word
pickIndices = flip flip 0 . go
  where
    go [] _ sc = sc
    go (i:is) xs sc = go is (sans i xs) $ sc + score (focus i xs)

indices :: Foldable t => t a -> [[Int]]
indices xs
  | len <= 2 = []
  | otherwise = mapM (enumFromTo 1) [len - 2,len - 3 .. 1]
  where len = length xs
{-# SPECIALIZE indices :: V.Vector Word -> [[Int]] #-}

focus :: Int -> V.Vector a -> [a]
focus i xs = map (V.unsafeIndex xs) [pred i, i, succ i]

scores :: V.Vector Word -> [([Int], Word)]
scores xs = map (\is -> (is, pickIndices is xs)) $ indices xs

maxScore :: V.Vector Word -> ([Int], Word)
maxScore = maximumBy (comparing snd) . scores
