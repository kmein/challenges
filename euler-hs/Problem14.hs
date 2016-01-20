module Main where
----------------------------------------------------
import Control.Arrow (first)
----------------------------------------------------
nextCollatz :: Int -> Int
nextCollatz n
    | even n = n `div` 2
    | odd n  = 3 * n + 1

collatzs :: Int -> ([Int], Int)
collatzs n = first (takeWhile (>= 1) . iterate nextCollatz) (n, n)

main :: IO ()
main = do
    let colls = map (first ((+1) . length . takeWhile (/= 1)) . collatzs) [1..999999]
    print $ maximum colls

