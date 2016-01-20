module Problem5 (main) where
--------------------------
{-
isCandidate :: Int -> Bool
divisible :: Int -> Int -> Bool
main :: IO ()
--------------------------
divisible n x = n `mod` x == 0

isCandidate n = not $ elem False $ map (divisible n) [1..20]

main = print $ head $ filter isCandidate [1..]
-}
main :: IO ()
main = print . foldr1 lcm $ [1..20]
