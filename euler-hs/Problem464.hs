module Problem464 where

import Utils.Math

squarefree :: Integral t
           => t -> Bool
squarefree x = any (`divides` x) [y ^ 2 | y <- [2 .. x]]

möbius :: Integral t
       => t -> t
möbius n
  | squarefree n = (-1) ^ omega n
  | otherwise = 0

omega :: Integral t
      => t -> Int
omega = length . primeFactors

p, n :: Integral b
     => (b,b) -> Int
p (a,b) = 
  length $
  filter (positive . möbius)
         [a .. b]

n (a,b) = 
  length $
  filter (negative . möbius)
         [a .. b]

c (n) = 
  length $
  [(a,b) | a <- [1 ..]
         , b <- [1 ..]
         , 1 <= a && a <= b && b <= n
         , 99 * n (a,b) <= 100 * p (a,b)
         , 99 * p (a,b) <= 100 * n (a,b)]-- TODO 
