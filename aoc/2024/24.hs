{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Bits
import Data.Graph
import Data.Map.Strict qualified
import Data.Maybe
import Data.Text qualified
import Data.Text.IO qualified
import Debug.Trace
import System.Environment

type Label = Data.Text.Text
type Inputs = Data.Map.Strict.Map Label Bool
type Node = (Inputs -> Inputs, Label, [Label])
type Circuit = (Graph, Vertex -> Node, Label -> Maybe Vertex)

getInput :: IO (Circuit, Inputs)
getInput = do
  inputString <- Data.Text.IO.readFile . maybe "24.txt" (const "24.txt.test") =<< lookupEnv "AOC_TEST"
  case Data.Text.splitOn "\n\n" inputString of
    [initialValuesString, gatesString] ->
      let initialValues = Data.Map.Strict.fromList $ map parseInitialValue $ Data.Text.lines initialValuesString
          graphTriple = graphFromEdges $ map parseGate $ Data.Text.lines gatesString
      in pure (graphTriple, initialValues)
    _ -> error "File needs to have two sections."
  where
    parseInitialValue line =
      let (identifier, rest) = Data.Text.splitAt 3 line
          (_, value) = Data.Text.splitAt 2 rest
      in (identifier, value == "1")
    parseGate line =
      case Data.Text.words line of
        [a, op, b, "->", c] -> (apply op a b c, c, [a, b])
        _ -> error "Malformed file."
      where
        apply op a b c m =
          let operator = case op of
                "XOR" -> (/=)
                "AND" -> (&&)
                "OR" -> (||)
                _ -> error "Malformed operation."
              a' = m Data.Map.Strict.! a
              b' = m Data.Map.Strict.! b
              c' = a' `operator` b'
          in
          trace
            (Data.Text.unpack $
              Data.Text.unwords
                [ a, Data.Text.pack ("(" ++ show a' ++ ")")
                , op
                , b, Data.Text.pack ("(" ++ show b' ++ ")")
                , "="
                , c, Data.Text.pack ("(" ++ show c' ++ ")")
                ]) $
          Data.Map.Strict.insert c c' m

readResult :: Inputs -> Int
readResult =
  Data.Map.Strict.foldrWithKey
    (\k v acc ->
      case Data.Text.uncons k of
        Just ('z', read . Data.Text.unpack -> bitIndex) -> (if v then setBit else clearBit) acc bitIndex
        _ -> acc
    )
    zeroBits

evaluateCircuit :: Circuit -> Inputs -> Inputs
evaluateCircuit (graph, getNode, _) initialValues =
  foldr (\vertex acc -> let (update, _, _) = getNode vertex in update acc) initialValues $ topSort graph

main :: IO ()
main = do
  (inputs, circuit) <- getInput
  print $ readResult $ evaluateCircuit inputs circuit
