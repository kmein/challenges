{-# LANGUAGE Strict #-}
import Control.Monad.State.Strict
import Data.List (maximumBy)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.Vector.Unboxed as V
import Data.MemoTrie (memo)

type Score = Word

unDigits :: Integral a => [a] -> a
unDigits = foldl (\a b -> a * 10 + b) 0
{-# SPECIALIZE unDigits :: [Word] -> Word #-}
{-# INLINE unDigits #-}

digits :: Integral a => a -> [a]
digits = dr []
  where
    dr acc 0 = acc
    dr acc x = dr (d : acc) r
        where (r, d) = quotRem x 10
{-# SPECIALIZE digits :: Word -> [Word] #-}

score :: [Word] -> Score
score = memo score'
    where score' = last . primeFactors . unDigits . concatMap digits

sans :: (V.Unbox a) => Int -> V.Vector a -> V.Vector a
sans i xs =
  let (ys, zs) = V.splitAt i xs
  in ys V.++ V.tail zs
{-# SPECIALIZE sans :: Int -> V.Vector Word -> V.Vector Word #-}

pickIndex :: Int -> State (V.Vector Word) Word
pickIndex i =
  let is = [pred i, i, succ i]
  in gets (\xs -> score $ map (xs V.!) is) <* modify (sans i)

pickIndices :: [Int] -> V.Vector Word -> Score
pickIndices is = sum . evalState (mapM pickIndex is)

indices :: Int -> [[Int]]
indices len =
  if len <= 2
    then []
    else mapM (enumFromTo 1) [len - 2,len - 3 .. 1]

scores :: V.Vector Word -> [([Int], Score)]
scores xs = map (\is -> (is, pickIndices is xs)) (indices $ V.length xs)

main :: IO ()
main = do
  n <- (read . head) <$> getArgs
  arr <- V.fromList <$> replicateM n (randomRIO (0, 99))
  print arr
  print $ maximumBy (comparing snd) $ scores arr
