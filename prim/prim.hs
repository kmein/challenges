{-# LANGUAGE TypeApplications #-}
import Control.Monad.State.Strict
import Data.List (maximumBy)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.Vector as V

type Score = Int

largestPrimeFactor :: Int -> Int
largestPrimeFactor = last . primeFactors

score :: [Int] -> Score
score = largestPrimeFactor . read . concatMap show

sans :: Int -> V.Vector a -> V.Vector a
sans i xs =
  let (ys, zs) = V.splitAt i xs
  in ys V.++ V.tail zs

pickIndex :: Int -> State (V.Vector Int) Int
pickIndex i =
  let is = [pred i, i, succ i]
  in gets (\xs -> score $ map (xs V.!) is) <* modify (sans i)

pickIndices :: [Int] -> V.Vector Int -> Score
pickIndices is = sum . evalState (mapM pickIndex is)

indices :: Int -> [[Int]]
indices len =
  if len <= 2
    then []
    else mapM (enumFromTo 1) [len - 2,len - 3 .. 1]

scores :: V.Vector Int -> [([Int], Score)]
scores xs = map (\is -> (is, pickIndices is xs)) (indices $ V.length xs)

main :: IO ()
main = do
  n <- (read @Int . head) <$> getArgs
  arr <- V.fromList <$> replicateM n (randomRIO (0, 99))
  print arr
  print $ maximumBy (comparing snd) $ scores arr
