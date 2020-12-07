module Main where

import Control.Monad (join)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Debug.Trace

parsePassport :: String -> [(String, String)]
parsePassport = map (\string -> let [k, v] = splitOn ":" string in (k, v)) . words

validOld :: [(String, String)] -> Bool
validOld passport =
  let keys = map fst passport
  in all (`elem` keys) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

yearValid :: (Int, Int) -> String -> Bool
yearValid (bottom, top) string =
  let year = read (takeWhile isDigit string)
  in bottom <= year && top >= year

heightValid string =
  case span isDigit string of
    (number, "cm") -> read number >= 150 && read number <= 193
    (number, "in") -> read number >= 59 && read number <= 76
    _ -> False

hairColorValid string =
  case string of
    '#':code@[_,_,_,_,_,_] -> all (`elem` "0123456789abcdef") code
    _ -> False

validNew passport
  | Just byr <- lookup "byr" passport, yearValid (1920, 2002) byr
  , Just iyr <- lookup "iyr" passport, yearValid (2010, 2020) iyr
  , Just eyr <- lookup "eyr" passport, yearValid (2020, 2030) eyr
  , Just hgt <- lookup "hgt" passport, heightValid hgt
  , Just hcl <- lookup "hcl" passport, hairColorValid hcl
  , Just ecl <- lookup "ecl" passport, ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  , Just pid <- lookup "pid" passport, length pid == 9, all isDigit pid
  = True
  | otherwise = False

main = do
  passports <- map parsePassport . splitOn "\n\n" <$> readFile "4.txt"
  print $ length $ filter validOld passports
  print $ length $ filter validNew passports
