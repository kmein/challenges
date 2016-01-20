module Problem7 (primes, primeFactors, main) where
---------------------------------------------------
primes :: [Int]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors :: Int -> [Int]
primeFactors n = factor n primes
    where
        factor x (p:ps)
            | p*p > x        = [x]
            | x `mod` p == 0 = p : factor (x `div` p) (p:ps)
            | otherwise      = factor x ps

main :: IO ()
main = print $ primes !! 10000



{-
isPrime :: Int -> Bool
factors :: Integral t => t -> [t]
main :: IO ()
---------------------
factors x = [n | n <- [1..x], x `mod` n == 0]

isPrime n = factors n == [1, n]

main = print $ (filter isPrime [2..]) !! 10000
-}
