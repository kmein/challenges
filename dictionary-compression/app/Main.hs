module Main where

import System.Environment (getArgs)
import DictionaryCompression (compress)

main :: IO ()
main = mapM_ print . compress . lines =<< readFile =<< (head <$> getArgs)
