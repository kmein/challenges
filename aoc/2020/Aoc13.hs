import Data.List (find, tails)
import Data.Bifunctor (second)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Safe (readMay)

type Schedule = [Maybe Int]

main = do
  (timestamp, schedule) <- parseFile "13.test.txt"
  let (departure, busID) = earliestDeparture timestamp schedule
  print ((departure - timestamp) * fromIntegral busID)
  print $ findSuccessiveDepartures 0 schedule

parseFile :: FilePath -> IO (Integer, Schedule)
parseFile path = do
  contents <- lines <$> readFile path
  case contents of
    [timestampStr, busesStr]
      | Just timestamp <- readMay timestampStr, buses <- map readMay (splitOn "," busesStr)
      -> return (timestamp, buses)
    _ -> error "file dont match yo format lol"

-- returns the bus departures for every minute (first element is for minute 0 etc)
departuresAt :: Integer -> Schedule -> [Int]
departuresAt minute = filter (\busId -> minute `mod` fromIntegral busId == 0) . catMaybes

earliestDeparture :: Integer -> Schedule -> (Integer, Int)
earliestDeparture timestamp schedule =
  case departuresAt timestamp schedule of
    [] -> earliestDeparture (succ timestamp) schedule
    x:_ -> (timestamp, x)

findSuccessiveDepartures :: Integer -> Schedule -> Integer
findSuccessiveDepartures startingTimestamp schedule =
  if and $ zipWith checkPosition schedule [startingTimestamp..]
     then startingTimestamp
     else findSuccessiveDepartures (succ startingTimestamp) schedule
  where
    checkPosition scheduled timeStamp =
      case scheduled of
        Just scheduled' -> departuresAt timeStamp schedule == [scheduled']
        Nothing -> True
