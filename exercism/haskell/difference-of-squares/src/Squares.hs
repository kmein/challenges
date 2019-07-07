module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference = (-) <$> squareOfSum <*> sumOfSquares

squareOfSum :: Integral a => a -> a
squareOfSum = (^2) . sum . enumFromTo 1

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map (^2) . enumFromTo 1
