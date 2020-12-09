{-# LANGUAGE LambdaCase #-}
import Data.Either (isRight)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Instruction = Nop Int | Acc Int | Jmp Int
  deriving (Show)

type Program = Map.Map Int Instruction

parseProgram :: Parsec Void String Program
parseProgram = Map.fromList . zip [0..] <$> some parseInstruction
  where
    parseInstruction :: Parsec Void String Instruction
    parseInstruction =
      let constructor = (Nop <$ string "nop") <|> (Acc <$ string "acc") <|> (Jmp <$ string "jmp")
          number = space *> signed (pure ()) decimal
      in (constructor <*> number) <* eol


-- Left : stopped before infinite loop
-- Right : terminated gracefully
run :: Program -> Either Int Int
run = go 0 0 Set.empty
  where
    go index accumulator visited instructions =
      if index `Set.member` visited
        then Left accumulator
        else case Map.lookup index instructions of
          Nothing -> Right accumulator
          Just (Nop _) -> go (index + 1) accumulator (Set.insert index visited) instructions
          Just (Acc n) -> go (index + 1) (accumulator + n) (Set.insert index visited) instructions
          Just (Jmp i) -> go (index + i) accumulator (Set.insert index visited) instructions

possibleFixes :: Program -> [Program]
possibleFixes program =
  let
    flipInstructionAt :: Int -> Program -> Program
    flipInstructionAt = Map.adjust $ \case
      Jmp x -> Nop x
      Nop x -> Jmp x
      a -> a
  in map (`flipInstructionAt` program) $ Map.keys program

main = do
  program' <- runParser parseProgram "o" <$> readFile "8.txt"
  case program' of
    Right program -> do
      print $ run program
      print $ filter isRight $ map run $ possibleFixes program
    Left error -> print error
