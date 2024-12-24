{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Bits
import Data.Graph
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import System.Environment

type Label = T.Text
type Inputs = M.Map Label Bool
type Gate =
  ( Inputs -> Inputs -- function
  , Label -- output label
  , [Label] -- input labels
  )

getInput :: IO ([Gate], Inputs)
getInput = do
  inputString <- T.readFile . maybe "24.txt" (const "24.txt.test") =<< lookupEnv "AOC_TEST"
  case T.splitOn "\n\n" inputString of
    [initialValuesString, gatesString] ->
      let initialValues = M.fromList $ map parseInitialValue $ T.lines initialValuesString
          gates = map parseGate $ T.lines gatesString
      in pure (gates, initialValues)
    _ -> error "File needs to have two sections."
  where
    parseInitialValue line =
      let (identifier, rest) = T.splitAt 3 line
          (_, value) = T.splitAt 2 rest
      in (identifier, value == "1")
    parseGate :: T.Text -> Gate
    parseGate line =
      case T.words line of
        [a, op, b, "->", c] -> (apply op a b c, c, [a, b])
        _ -> error "Malformed file."
      where
        apply op a b c m =
          let a' = m M.! a
              b' = m M.! b
              c' = case op of
                "XOR" -> a' /= b'
                "AND" -> a' && b'
                "OR"  -> a' || b'
                _     -> error "Malformed expression."
              debug = T.unpack a <> " (" <> show a' <> ") " <> T.unpack op <> " " <> T.unpack b <> " (" <> show b' <> ") = " <> T.unpack c <> " (" <> show c' <> ")"
          in trace debug $ M.insert c c' m

readBinary :: Char -> Inputs -> Int
readBinary variable =
  M.foldrWithKey
    (\k v acc ->
      case T.uncons k of
        Just (c, read . T.unpack -> bitIndex) | c == variable -> (if v then setBit else clearBit) acc bitIndex
        _ -> acc
    )
    zeroBits

evaluateCircuit :: [Gate] -> Inputs -> Inputs
evaluateCircuit gates initialInputs =
  let (graph, getGate, _) = graphFromEdges gates
  in foldr (\vertex inputs -> let (runGate, _, _) = getGate vertex in runGate inputs) initialInputs $ topSort graph

main :: IO ()
main = do
  (gates, inputs) <- getInput
  print $ readBinary 'z' $ evaluateCircuit gates inputs
