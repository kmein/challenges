#!/usr/bin/env stack
-- stack --resolver lts-9.17 runhaskell --package tasty --package tasty-hunit --package vector --package split

import Control.Monad
import Data.Array (assocs)
import Data.Char
import Data.Graph
import Data.List
import Data.List.Split
import Data.Tree (subForest)
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace (traceShow)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM (modify)

data Half = Fst | Snd

half Fst f _ = f
half Snd _ g = g

testDay n h1 h2 = testGroup (show n) [testGroup "*" h1, testGroup "**" h2]

day1 h = sum . map fst . filter (uncurry (==)) . half h withNext withHalfway
  where
    withNext xs = xs `zip` tail (cycle xs)
    withHalfway xs = xs `zip` drop (length xs `div` 2) (cycle xs)

day2 h = sum . map (half h difference quotient)
  where
    difference row = maximum row - minimum row
    quotient row =
        head
            [ x `div` y
            | x <- row
            , y <- row
            , x `mod` y == 0
            , x /= y ]

day4 h xs = ws == half h nub (nubBy anagram) ws
  where
    anagram x y = sort x == sort y
    ws = words xs

day5 h = loop 0 0
  where
    loop count i xs
        | V.length xs == i = count
        | otherwise =
            loop
                (succ count)
                (i + (xs V.! i))
                (V.modify (\v -> VM.modify v (half h succ strange) i) xs)
    strange x
        | x >= 3 = x - 1
        | otherwise = x + 1

day6 h = half h fst snd . countLoop . iterate step
  where
    step v =
        V.accum
            (+)
            v'
            [ (i `mod` V.length v, 1)
            | i <- [mi + 1 .. mi + m] ]
      where
        mi = V.maxIndex v
        m = v V.! mi
        v' = v V.// [(mi, 0)]
    countLoop = go 0 M.empty
      where
        go _ _ [] = error "Infinite list expected."
        go n m (x:xs) =
            case M.lookup x m of
                Just l -> (n, l)
                Nothing -> go (n + 1) (M.insert x 1 m') xs
          where
            m' = succ <$> m

day7 inp = programName $ topProgram g
  where
    -- subWeights v = map (sum . fmap programWeight) $ subForest $ head $ dfs g [v]
    -- programWeight = (\((_, w), _, _) -> w) . getProgram
    programName = (\(_, p, _) -> p) . getProgram
    topProgram = fromJust . fmap fst . find ((== 0) . snd) . assocs . indegree
    (g, getProgram, getVertex) =
        graphFromEdges $ map ((\(n, w, c) -> ((n, w), n, c)) . parse) inp
    parse :: String -> (String, Integer, [String])
    parse s =
        let (name, ' ':'(':s') = span isAlpha s
            (weight, ')':s'') = span isNumber s'
        in case stripPrefix " -> " s'' of
               Nothing -> (name, read weight, [])
               Just s''' -> (name, read weight, splitOn ", " s''')

day8 h ins = half h (maximum $ foldl apply M.empty $ map parse ins) undefined
  where
    apply regs (reg, (op, opand), (condReg, condOrds, condVal)) =
        if compare (fromMaybe 0 $ M.lookup condReg regs) condVal `elem` condOrds
            then case op of
                     "inc" -> M.insertWith (+) reg opand regs
                     "dec" -> M.insertWith (-) reg opand regs
            else regs
    ords c =
        case c of
            "==" -> [EQ]
            "!=" -> [LT, GT]
            "<" -> [LT]
            ">" -> [GT]
            "<=" -> [LT, EQ]
            ">=" -> [GT, EQ]
    parse :: String -> (String, (String, Int), (String, [Ordering], Int))
    parse s =
        let [reg, op, opand, _, condReg, condComp, condVal] = words s
        in (reg, (op, read opand), (condReg, ords condComp, read condVal))

main :: IO ()
main = do
    let parse = read :: String -> Int
    day1input <- (map digitToInt . filter isDigit) <$> readFile "day1.txt"
    day2input <- (map (map parse . words) . lines) <$> readFile "day2.txt"
    day4input <- lines <$> readFile "day4.txt"
    day5input <- (fmap parse . V.fromList . lines) <$> readFile "day5.txt"
    day6input <- (fmap parse . V.fromList . words) <$> readFile "day6.txt"
    day7input <- lines <$> readFile "day7.txt"
    day8input <- lines <$> readFile "day8.txt"
    defaultMain
        (testGroup
             "Tests"
             [ testDay
                   1
                   [ testCase "1" $ day1 Fst [1, 1, 2, 2] @?= 3
                   , testCase "2" $ day1 Fst [1, 1, 1, 1] @?= 4
                   , testCase "3" $ day1 Fst [1, 2, 3, 4] @?= 0
                   , testCase "4" $ day1 Fst [9, 1, 2, 1, 2, 1, 2, 9] @?= 9
                     -- , testCase "#" $ day1 Fst day1input @?= 0 -- 1171
                    ]
                   [ testCase "1" $ day1 Snd [1, 2, 1, 2] @?= 6
                   , testCase "2" $ day1 Snd [1, 2, 2, 1] @?= 0
                   , testCase "3" $ day1 Snd [1, 2, 3, 4, 2, 5] @?= 4
                   , testCase "4" $ day1 Snd [1, 2, 3, 1, 2, 3] @?= 12
                   , testCase "5" $ day1 Snd [1, 2, 1, 3, 1, 4, 1, 5] @?= 4
                     -- , testCase "#" $ day1 Snd day1input @?= 0 -- 1024
                    ]
             , testDay
                   2
                   [ testCase "1" $
                     day2 Fst [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]] @?= 18
                     -- , testCase "#" $ day2 Fst day2input @?= 0 -- 54426
                    ]
                   [ testCase "1" $
                     day2 Snd [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]] @?= 9
                     -- , testCase "#" $ day2 Snd day2input @?= 0 -- 333
                    ]
             , testDay
                   4
                   [ testCase "1" $ day4 Fst "aa bb cc dd ee" @?= True
                   , testCase "2" $ day4 Fst "aa bb cc dd aa" @?= False
                   , testCase "3" $ day4 Fst "aa bb cc dd aaa" @?= True
                     -- , testCase "#" $ length (filter day4 Fst day4input) @?= 0 -- 466
                    ]
                   [ testCase "1" $ day4 Snd "abcde fghij" @?= True
                   , testCase "2" $ day4 Snd "abcde xyz ecdab" @?= False
                   , testCase "3" $ day4 Snd "a ab abc abd abf abj" @?= True
                   , testCase "4" $ day4 Snd "iiii oiii ooii oooi oooo" @?= True
                   , testCase "5" $ day4 Snd "oiii ioii iioi iiio" @?= False
                     -- , testCase "#" $ length (filter day4 Snd day4input) @?= 0 -- 251
                    ]
             , testDay
                   5
                   [ testCase "1" $ day5 Fst (V.fromList [0, 3, 0, 1, -3]) @?= 5
                     -- , testCase "#" $ day5 Fst day5input @?= 0 -- 318883 (took 15s)
                    ]
                   [ testCase "1" $ day5 Snd (V.fromList [0, 3, 0, 1, -3]) @?= 10
                     -- , testCase "#" $ day5 Snd day5input @?= 0 -- 23948711 (took 69s)
                    ]
             , testDay
                   6
                   [ testCase "1" $ day6 Fst (V.fromList [0, 2, 7, 0]) @?= 5
                     -- , testCase "#" $ day6 Fst day6input @?= 0] -- 6681
                    ]
                   [ testCase "1" $ day6 Snd (V.fromList [0, 2, 7, 0]) @?= 4
                     -- , testCase "#" $ day6 Snd day6input @?= 0 -- 2392
                    ]
             , let day7test =
                       lines
                           "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
               in testDay
                      7
                      [ testCase "1" $ day7 day7test @?= "tknk"
                        -- , testCase "#" $ day7 Fst day7input @?= [] -- vmpywg
                       ]
                      []
             , let day8test =
                       lines
                           "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
               in testDay
                      8
                      [ testCase "1" $ day8 Fst day8test @?= 1
                      , testCase "#" $ day8 Fst day8input @?= 0]
                      []])
