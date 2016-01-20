module Problem40 where

champernowne :: String
champernowne = "0." ++ concatMap show ([1..] :: [Integer])

fractionalPart :: String
fractionalPart = drop 2 champernowne

nthFractional :: Int -> String
nthFractional n = [fractionalPart !! (n + 1)]

d :: Int -> Int
d n = read (nthFractional n) :: Int

main :: IO ()
main = print $ product [d 1, d 10, d 100, d 1000, d 10000, d 100000, d 1000000]
