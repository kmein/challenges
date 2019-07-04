{-# LANGUAGE OverloadedLists #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts dna
  | valid dna = Right $ foldr (\n -> insertWith (+) (classify n) 1) empty dna
  | otherwise = Left dna
  where classify = read . pure
        empty = [(A,0),(C,0),(G,0),(T,0)]
        valid = all (`elem` "ACGT")
