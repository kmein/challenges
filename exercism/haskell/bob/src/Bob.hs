module Bob (responseFor) where

import Data.Char
import Data.List (dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

responseFor :: String -> String
responseFor bob
  | null xs = "Fine. Be that way!"
  | shouted xs && question xs = "Calm down, I know what I'm doing!"
  | question xs = "Sure."
  | shouted xs = "Whoa, chill out!"
  | otherwise = "Whatever."
  where 
    xs = trim bob
    question = (== '?') . last 
    shouted xs = 
      let letters = filter isAlpha xs
      in not (null letters) && all isUpper letters
