import Control.Monad (foldM)
import Data.Bits (setBit, clearBit, Bits)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Map as Map
import Data.Functor.Identity

type Mask = [Maybe Bool]
type Memory = Map.Map Address Integer
type Address = Word

data Instruction = SetMask Mask | SetMemory Address Integer
  deriving (Show)

parseProgram :: Parsec Void String [Instruction]
parseProgram = some $ (try setMask <|> setMemory) <* eol
  where
    setMask :: Parsec Void String Instruction
    setMask =
      SetMask <$>
        (string "mask = " *> some (foldl1 (<|>) [Just False <$ char '0', Just True <$ char '1', Nothing <$ char 'X']))
    setMemory = SetMemory <$> (string "mem[" *> decimal <* string "] = ") <*> decimal

setBits :: (Applicative f, Bits a) => a -> (Int, Maybe Bool) -> f a
setBits number (bit, value) =
  pure $
  case value of
    Just True -> setBit number bit
    Just False -> clearBit number bit
    Nothing -> number

floatBits :: (Bits a) => a -> (Int, Maybe Bool) -> [a]
floatBits number (bit, value) =
  case value of
    Just True -> pure $ setBit number bit
    Just False -> pure number
    Nothing -> [setBit number bit, clearBit number bit]

applyMask :: (Monad f, Bits a) => (a -> (Int, Maybe Bool) -> f a) -> Mask -> a -> f a
applyMask f mask number = foldM f number $ zip [0..] (reverse mask)

run :: (Maybe Mask -> Address -> Integer -> Memory -> Memory) -> [Instruction] -> Integer
run mutateMemory = sum . go (Map.empty, Nothing)
  where
    go (memory, mask) instructions =
      case instructions of
        [] -> memory
        SetMask mask':rest -> go (memory, Just mask') rest
        SetMemory addr value:rest -> go (mutateMemory mask addr value memory, mask) rest

mutateOld :: Maybe Mask -> Address -> Integer -> Memory -> Memory
mutateOld mask addr value = Map.insert addr value'
  where value' = maybe value (\mask -> runIdentity $ applyMask setBits mask value) mask

mutateNew :: Maybe Mask -> Address -> Integer -> Memory -> Memory
mutateNew mask addr value memory = foldl (\m a -> Map.insert a value m) memory addrs
  where addrs = maybe [addr] (\mask -> applyMask floatBits mask addr) mask

main = do
  code <- readFile "14.txt"
  case runParser parseProgram "file" code of
    Right program -> do
      print $ run mutateOld program
      print $ run mutateNew program
    Left e -> print e
