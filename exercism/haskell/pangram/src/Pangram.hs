module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram word = all (`elem` lowerWord) ['a'..'z']
  where lowerWord = map toLower word
