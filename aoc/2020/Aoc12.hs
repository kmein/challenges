{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens ((&), makeLenses, (%~), (+~), (-~), (^.), (.~))
import Data.List (uncons)

data Direction = West | North | East | South
 deriving (Show)

code :: Direction -> Char
code = \case
  West -> 'W'
  North -> 'N'
  East -> 'E'
  South -> 'S'

instance Enum Direction where
  fromEnum = \case
    West -> 0
    North -> 1
    East -> 2
    South -> 3
  toEnum x = case x `mod` 4 of
    0 -> West
    1 -> North
    2 -> East
    3 -> South
    _ -> error "unreachable"

data Ship = Ship
  { _direction :: Direction
  , _unitsEast :: Int
  , _unitsSouth :: Int
  } deriving (Show)
$(makeLenses ''Ship)

data Waypoint = Waypoint
  { _waypointEast :: Int
  , _waypointNorth :: Int
  } deriving (Show)
$(makeLenses ''Waypoint)

apply :: Int -> (a -> a) -> a -> a
apply n f x
  | n < 1 = x
  | otherwise = apply (n - 1) f (f x)

spinShip :: Either Int Int -> Direction -> Direction
spinShip leftOrRight startingDirection =
  case leftOrRight of
    Left degrees -> apply (degrees `div` 90) pred startingDirection
    Right degrees -> apply (degrees `div` 90) succ startingDirection

spinWaypoint :: Either Int Int -> Waypoint -> Waypoint
spinWaypoint leftOrRight =
  case leftOrRight of
    Left degrees -> apply (degrees `div` 90) leftTurn
    Right degrees -> apply (degrees `div` 90) rightTurn
  where
    leftTurn wp =
      wp & waypointNorth .~ (wp ^. waypointEast)
         & waypointEast .~ negate (wp ^. waypointNorth)
    rightTurn wp =
      wp & waypointNorth .~ negate (wp ^. waypointEast)
         & waypointEast .~ (wp ^. waypointNorth)


runShip :: [String] -> Ship
runShip = foldl (flip update) (Ship East 0 0)
  where
    update command ship =
      case uncons command of
        Just (dir, rest) ->
          let number = read rest
           in case dir of
                'L' -> ship & direction %~ spinShip (Left number)
                'R' -> ship & direction %~ spinShip (Right number)
                'N' -> ship & unitsSouth -~ number
                'S' -> ship & unitsSouth +~ number
                'W' -> ship & unitsEast -~ number
                'E' -> ship & unitsEast +~ number
                'F' -> update (code (ship ^. direction) : rest) ship
        Nothing -> error ("update: malformed command: " ++ show command)

runWaypoint :: [String] -> (Ship, Waypoint)
runWaypoint = foldl (flip update) (Ship East 0 0, Waypoint 10 1)
  where
    update command (ship, waypoint) =
      case command of
        dir:rest -> let number = read rest in
          case dir of
            'L' -> (ship, waypoint & spinWaypoint (Left number))
            'R' -> (ship, waypoint & spinWaypoint (Right number))
            'N' -> (ship, waypoint & waypointNorth +~ number)
            'S' -> (ship, waypoint & waypointNorth -~ number)
            'W' -> (ship, waypoint & waypointEast -~ number)
            'E' -> (ship, waypoint & waypointEast +~ number)
            'F' ->
              ( ship
                & unitsSouth -~ number * (waypoint ^. waypointNorth)
                & unitsEast +~ number * (waypoint ^. waypointEast)
              , waypoint
              )

manhattanDistance :: Ship -> Int
manhattanDistance state = abs (state ^. unitsSouth) + abs (state ^. unitsEast)

main = do
  commands <- lines <$> readFile "12.txt"
  print $ manhattanDistance $ runShip commands
  print $ manhattanDistance $ fst $ runWaypoint commands
