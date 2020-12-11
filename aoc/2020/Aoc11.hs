import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import Data.Function (on)

type Grid a = Map.Map (Int, Int) a

display :: Grid Char -> String
display grid = unlines $ map (map snd) $ groupBy ((==) `on` (fst.fst)) $ Map.assocs grid

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

adjacents :: (Int, Int) -> Grid a -> [a]
adjacents point grid = mapMaybe (`Map.lookup` grid) (diagonals point)
  where diagonals (x, y) = [(x+n, y+m) | n <- [-1..1], m <- [-1..1], (n, m) /= (0, 0)]

visibleAdjacents :: (Int, Int) -> Grid Char -> [Char]
visibleAdjacents (x, y) grid =
    concatMap (firstSeat . getSpaces)
      [ [(x+n, y) | n <- range] -- +x
      , [(x-n, y) | n <- range] -- -x
      , [(x, y+n) | n <- range] -- +y
      , [(x, y-n) | n <- range] -- -y
      , [(x+n,y+n) | n <- range] -- +x +y
      , [(x+n,y-n) | n <- range] -- +x -y
      , [(x-n,y-n) | n <- range] -- -x -y
      , [(x-n,y+n) | n <- range] -- -x +y
      ]
  where ((minX, minY), _) = Map.findMin grid
        ((maxX, maxY), _) = Map.findMax grid
        range = [1..max maxX maxY]
        firstSeat = take 1 . dropWhile (== '.')
        getSpaces = mapMaybe (`Map.lookup` grid)


update :: ((Int, Int) -> Grid Char -> [Char]) -> Int -> Grid Char -> Grid Char
update adjacentsFunction threshold grid = Map.mapWithKey updateSpace grid
  where
    updateSpace point value = case (value, count '#' (adjacentsFunction point grid)) of
      ('L', 0) -> '#'
      ('#', n) | n >= threshold -> 'L'
      _ -> value

updateOld = update adjacents 4
updateNew = update visibleAdjacents 5

fixWith :: Eq a => (a -> a) -> a -> a
fixWith f x = if x == f x then x else fixWith f (f x)

gridify :: [[a]] -> Grid a
gridify =
  Map.fromList
  . concatMap (\(i, xs) -> map (\(j, x) -> ((i, j), x)) xs) . zip [0..]
  . map (zip [0..])

main = do
  seats <- gridify . lines <$> readFile "11.txt"
  print $ Map.size $ Map.filter (== '#') $ fixWith updateOld seats
  print $ Map.size $ Map.filter (== '#') $ fixWith updateNew seats
