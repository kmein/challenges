{-# LANGUAGE LambdaCase #-}
import Data.List (nub)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Map as Map

type Rule = (String, [(Int, String)])

parseRule :: Parsec Void String Rule
parseRule =
  let
    twoAdjectives = (++) <$> some letterChar <*> (space *> some letterChar)
    subBag = (,) <$> decimal <*> (space *> twoAdjectives <* string " bag" <* optional (char 's'))
  in do
    adjectives <- twoAdjectives
    string " bags contain "
    subBags <- try ([] <$ string "no other bags") <|> (subBag `sepBy` string ", ")
    char '.' *> eol
    return (adjectives, subBags)

bagsAround :: String -> [Rule] -> [String]
bagsAround color allBags =
  let
    holdsBag color = any ((== color) . snd) . snd
    colors = map fst $ filter (holdsBag color) allBags
  in colors ++ concatMap (`bagsAround` allBags) colors

bagsIn :: String -> [Rule] -> Int
bagsIn color allBags =
  case lookup color allBags of
    Just subBags ->
      sum (map (\(amount, color) -> amount + amount * bagsIn color allBags) subBags)
    _ -> error "malformed rules lol"

main = readFile "7.txt"
  >>= return . parseMaybe (some parseRule)
  >>= \case
    Just allBags -> do
      print $ length $ nub $ bagsAround "shinygold" allBags
      print $ bagsIn "shinygold" allBags
    _ -> putStrLn "parse failed"
