module Problem10 (main) where
--------------------------
import Problem3 (primes)
--------------------------
main :: IO ()
main = print $ sum $ takeWhile (< 2000000) primes
-- 142913828922