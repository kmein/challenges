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
type Node = (Inputs -> Inputs, Label, [Label])
type Circuit = (Graph, Vertex -> Node, Label -> Maybe Vertex)

getInput :: IO (Circuit, Inputs)
getInput = do
  inputString <- T.readFile . maybe "24.txt" (const "24.txt.test") =<< lookupEnv "AOC_TEST"
  case T.splitOn "\n\n" inputString of
    [initialValuesString, gatesString] ->
      let initialValues = M.fromList $ map parseInitialValue $ T.lines initialValuesString
          graphTriple = graphFromEdges $ map parseGate $ T.lines gatesString
      in pure (graphTriple, initialValues)
    _ -> error "File needs to have two sections."
  where
    parseInitialValue line =
      let (identifier, rest) = T.splitAt 3 line
          (_, value) = T.splitAt 2 rest
      in (identifier, value == "1")
    parseGate line =
      case T.words line of
        [a, op, b, "->", c] -> (apply op a b c, c, [a, b])
        _ -> error "Malformed file."
      where
        apply op a b c m =
          let operator = case op of
                "XOR" -> (/=)
                "AND" -> (&&)
                "OR" -> (||)
                _ -> error "Malformed operation."
              a' = m M.! a
              b' = m M.! b
              c' = a' `operator` b'
          in
          trace
            (T.unpack $
              T.unwords
                [ a, T.pack ("(" ++ show a' ++ ")")
                , op
                , b, T.pack ("(" ++ show b' ++ ")")
                , "="
                , c, T.pack ("(" ++ show c' ++ ")")
                ]) $
          M.insert c c' m

readBinary :: Char -> Inputs -> Int
readBinary variable =
  M.foldrWithKey
    (\k v acc ->
      case T.uncons k of
        Just (c, read . T.unpack -> bitIndex) | c == variable -> (if v then setBit else clearBit) acc bitIndex
        _ -> acc
    )
    zeroBits

evaluateCircuit :: Circuit -> Inputs -> Inputs
evaluateCircuit (graph, getNode, _) initialValues =
  foldr (\vertex acc -> let (update, _, _) = getNode vertex in update acc) initialValues $ topSort graph

main :: IO ()
main = do
  (circuit, inputs) <- getInput
  print $ readBinary 'x' inputs
  print $ readBinary 'y' inputs
  print $ readBinary 'z' $ evaluateCircuit circuit inputs
