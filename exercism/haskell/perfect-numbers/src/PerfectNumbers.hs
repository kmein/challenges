{-# LANGUAGE LambdaCase #-}
module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> Maybe [Int]
factors n
  | n < 1 = Nothing
  | otherwise = Just $ filter (\x -> n `mod` x == 0) [1..pred n]

classify :: Int -> Maybe Classification
classify n = orderingToClassification . flip compare n . sum <$> factors n
  where
    orderingToClassification = \case
      EQ -> Perfect
      GT -> Abundant
      LT -> Deficient
