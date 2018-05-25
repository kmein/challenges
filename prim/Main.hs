import Control.Monad (replicateM)
import System.Environment (getArgs)
import System.Random (randomRIO)
import qualified Data.Vector as V (fromList)

import Prim (maxScore)

main :: IO ()
main = do
  [n] <- map read <$> getArgs
  arr <- V.fromList <$> replicateM n (randomRIO (0, 99))
  print arr
  print $ maxScore arr
