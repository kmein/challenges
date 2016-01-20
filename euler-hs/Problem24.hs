module Problem24 (main) where
-----------------------------------
import Data.List (permutations, sort)
-----------------------------------
main :: IO ()
main = print $ (sort . permutations $ "0123456789") !! 999999