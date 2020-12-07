module Main where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Policy
  = Policy
  { number1 :: Int
  , number2 :: Int
  , letter :: Char
  } deriving Show

parse :: String -> Maybe (Policy, String)
parse line
  | [policy, password] <- splitOn ": " line
  , [occurrences, [letter]] <- splitOn " " policy
  , [occurrencesMin, occurrencesMax] <- splitOn "-" occurrences
  = Just (Policy
    { number2 = read occurrencesMax
    , number1 = read occurrencesMin
    , letter = letter
    }, password)
  | otherwise = Nothing

count :: Eq a => a -> [a] -> Int
count letter = length . filter (== letter)

-- number1 : min occurrences
-- number2 : max occurrences
validOld :: Policy -> String -> Bool
validOld policy password =
  let letterCount = count (letter policy) password
  in letterCount >= number1 policy && letterCount <= number2 policy

validNew :: Policy -> String -> Bool
validNew policy password =
  let (index1, index2) = (number1 policy - 1, number2 policy - 1) -- first index != 0
      indexValid index = password !! index == letter policy
  in (indexValid index1 && not (indexValid index2))
    || (not (indexValid index1) && indexValid index2)

main = do
  input <- mapMaybe parse . lines <$> readFile "2.txt"
  print $ length $ filter (uncurry validOld) input
  print $ length $ filter (uncurry validNew) input
