import Data.List (uncons, transpose, permutations)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Ticket = [Int]

data Rule = Rule { name :: String, ranges :: [(Int, Int)] }
  deriving Show

validRulePermutations :: [Ticket] -> [Rule] -> [[Rule]]
validRulePermutations tickets = filter worksWithTickets . permutations
  where
    columns = transpose tickets
    worksWithTickets permutation =
      and $ zipWith (all . matchesRule) permutation columns
    matchesRule rule value = any (\(mini, maxi) -> mini <= value && value <= maxi) (ranges rule)

isTicketValid :: [Rule] -> Ticket -> Bool
isTicketValid rules = all (isNumberValid rules)

isNumberValid :: [Rule] -> Int -> Bool
isNumberValid rules number = any (any (number `inRange`) . ranges) rules
  where number `inRange` (x,y) = x <= number && number <= y

parseTickets :: String -> Maybe (Ticket, [Ticket])
parseTickets = uncons . mapMaybe parseList . lines
  where
    parseList line
      | ',' `elem` line = Just $ map read $ splitOn "," line
      | otherwise = Nothing

parseRules :: String -> [Rule]
parseRules = mapMaybe parseRule . lines
  where
    parseRange string =
      case splitOn "-" string of
        [x, y] -> Just (read x, read y)
        _ -> Nothing
    parseRule :: String -> Maybe Rule
    parseRule line = case span (/= ':') line of
      (name, ':':' ':ranges) -> Just $ Rule name $ mapMaybe parseRange (splitOn " or " ranges)
      _ -> Nothing

main = do
  inputString <- readFile "16.txt"
  let rules = parseRules inputString
  case parseTickets inputString of
    Just (myTicket, nearbyTickets) -> do
      print $ sum $ filter (not . isNumberValid rules) $ concat nearbyTickets
      print $ validRulePermutations (filter (isTicketValid rules) nearbyTickets) rules
    Nothing -> putStrLn "no tickets found lol"
