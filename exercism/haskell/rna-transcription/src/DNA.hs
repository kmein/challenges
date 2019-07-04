{-# LANGUAGE LambdaCase #-}
module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = \case
  [] -> Right []
  'G':dna -> ('C':) <$> toRNA dna
  'C':dna -> ('G':) <$> toRNA dna
  'T':dna -> ('A':) <$> toRNA dna
  'A':dna -> ('U':) <$> toRNA dna
  x:_ -> Left x

