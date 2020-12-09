import Data.List (tails, subsequences, find, inits)
import Data.Maybe (isJust, mapMaybe)
import Data.List.Extra (unsnoc)

-- ref https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell#comment43882101_27733778
slidingWindows :: Int -> [a] -> [[a]]
slidingWindows m = foldr (zipWith (:)) (repeat []) . take m . tails

-- return the first number in a list that cannot be expressed as the sum of two of the previous `preambleLength` elements
nonMatching :: Int -> [Int] -> [Int]
nonMatching preambleLength numbers =
  flip mapMaybe (slidingWindows (preambleLength + 1) numbers) $ \window ->
    case unsnoc window of -- first `preambleLength` of window are preamble, last one is the number to be examined
      Nothing -> Nothing
      Just (preamble, number) ->
        if null [(a, b) | a <- preamble, b <- preamble, a + b == number, a /= b]
           then Just number
           else Nothing

contiguousSummands :: Int -> [Int] -> [[Int]]
contiguousSummands theSum =
  let predicate xs = length xs >= 2 && sum xs == theSum
   in mapMaybe (find predicate . inits) . tails

main = do
  numbers <- map read . lines <$> readFile "9.txt"
  case nonMatching 25 numbers of
    erroneousNumber:_ -> do
      print erroneousNumber
      print $
        let summands = head $ contiguousSummands erroneousNumber numbers
         in maximum summands + minimum summands
    _ -> putStrLn "nothing found lol"
