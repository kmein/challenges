module Problem1 (main) where
-------------------------------------------------------------
main :: IO ()
main = print $ sum [n | n <- [0..999] :: [Int], multipleOf n (3, 5)]
    where multipleOf n (x, y) = (n `mod` x == 0) || (n `mod` y == 0)

