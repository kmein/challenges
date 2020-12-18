{-# LANGUAGE LambdaCase #-}
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Op = Add | Multiply
  deriving (Show)

data Expression = Constant Integer | Binary Op Expression Expression
  deriving (Show)

lexer = Token.makeTokenParser $ emptyDef
  { Token.reservedOpNames = ["+", "*"]
  }

expression :: OperatorTable Char () Expression -> Parser Expression
expression op = buildExpressionParser op (term op)

operatorsOld =
  [[Infix (Binary Multiply <$ Token.reservedOp lexer "*") AssocLeft
   , Infix (Binary Add <$ Token.reservedOp lexer "+") AssocLeft
  ]]

operatorsNew =
  [ [Infix (Binary Add <$ Token.reservedOp lexer "+") AssocLeft]
  , [Infix (Binary Multiply <$ Token.reservedOp lexer "*") AssocLeft]
  ]

term op = Token.parens lexer (expression op) <|> fmap Constant (Token.integer lexer)

evaluate :: Expression -> Integer
evaluate = \case
  Constant x -> x
  Binary Add x y -> evaluate x + evaluate y
  Binary Multiply x y -> evaluate x * evaluate y

main = do
  string <- readFile "18.txt"
  case parse (many1 (expression operatorsOld)) "18.txt" string of
    Right terms -> print $ sum $ map evaluate terms
    Left e -> print e
  case parse (many1 (expression operatorsNew)) "18.txt" string of
    Right terms -> print $ sum $ map evaluate terms
    Left e -> print e
