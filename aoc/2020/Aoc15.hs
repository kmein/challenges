import Data.List (elemIndex)
import Data.List.Split (splitOn)
import qualified Data.IntMap as IntMap

-- https://www.youtube.com/watch?v=vbwHF1mmzZE

go :: [Int] -> [Int]
go input = loop (length input) (last input) $ IntMap.fromList $ zip (init input) [1..]
  where
    loop turn number history =
      case IntMap.lookup number history of
        Nothing -> 0 : loop (turn+1) 0 (IntMap.insert number turn history)
        Just lastTurn -> (turn-lastTurn) : loop (turn+1) (turn-lastTurn) (IntMap.insert number turn history)

main = do
  startingNumbers <- map read . splitOn "," <$> readFile "15.txt"
  print $ go startingNumbers !! (2020 - 1 - length startingNumbers)
  print $ go startingNumbers !! (30000000 - 1 - length startingNumbers)
