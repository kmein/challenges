module Problem3 (primes, primeFactors, main) where
-----------------------------------------
primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3, 5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor x (p:ps)
        | p * p > x      = [x]
        | x `mod` p == 0 = p : factor (x `div` p) (p:ps)
        | otherwise      = factor x ps

main :: IO ()
main = print . last . primeFactors $ 600851475143
