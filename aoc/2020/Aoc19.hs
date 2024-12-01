{-# LANGUAGE LambdaCase #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import qualified Data.IntMap as IntMap

main = print 32

type Definition = IntMap.IntMap [[Term]]
data Term = Literal Char | RuleRef Int
  deriving Show

syntax :: Parsec Void String Definition
syntax = IntMap.fromList <$> (rule `sepEndBy1` lineEnd) <* eof
  where
    lineEnd = space *> newline *> space -- at least one newline
    rule = (,) <$> (space *> decimal <* space) <*> (string ": " *> space *> expression)
      where
        expression = list `sepBy1` (space *> char '|' *> space)
        list = term `sepEndBy1` space
        term = literal <|> RuleRef <$> decimal
        literal = Literal <$> (char '"' *> anySingle <* char '"')

generateParser :: Int -> Definition -> Parsec Void String String
generateParser index definition =
  case IntMap.lookup index definition of
    Just alternatives -> foldl1 (<|>) (concat $ mapM (map parseTerm) alternatives)
    Nothing -> error "oh no"
  where
    parseTerm :: Term -> Parsec Void String String
    parseTerm = \case
      Literal ch -> string [ch]
      RuleRef index -> generateParser index definition
