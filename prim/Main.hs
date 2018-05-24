import Control.Monad (replicateM)
import Data.List (maximumBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.Vector.Unboxed as V (fromList)

import Prim (scores)

main :: IO ()
main = do
  [n] <- map read <$> getArgs
  arr <- V.fromList <$> replicateM n (randomRIO (0, 99))
  print arr
  print $ maximumBy (comparing snd) $ scores arr
