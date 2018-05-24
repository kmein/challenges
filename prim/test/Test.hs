{-# LANGUAGE OverloadedLists, ScopedTypeVariables, TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Vector.Unboxed as V

import Prim

instance (V.Unbox a, Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

main :: IO ()
main =
  defaultMain $
  testGroup
    "Tests"
    [ testGroup
        "digits"
        [ testCase "digits" $ [1, 3, 3, 7] @=? digits @Int 1337
        , testCase "unDigits" $ 1337 @=? unDigits @Int [1, 3, 3, 7]
        , testProperty "digits . unDigits" $ \(n :: Word) ->
            unDigits (digits n) == n
        , testProperty "unDigits . digits" $
          forAll (resize 9 $ listOf1 $ choose (0, 9)) $ \(ds :: [Word]) ->
            head ds /= 0 ==> digits (unDigits ds) == ds
        ]
    , testGroup
        "score"
        [ testCase "1" $ 27329 @=? score [27, 32, 9]
        , testCase "2" $ 3491 @=? score [27, 9, 28]
        , testCase "3" $ 7 @=? score [32, 9, 28]
        , testCase "4" $ 22769 @=? score [27, 32, 28]
        ]
    , testGroup
        "sans"
        [ testCase "1" $ [1, 3, 4] @=? sans @Int 1 [1, 2, 3, 4]
        , testProperty "tail" $ \(xs :: V.Vector Int) ->
            not (V.null xs) ==> sans 0 xs == V.tail xs
        , testProperty "init" $ \(xs :: V.Vector Int) ->
            not (V.null xs) ==> sans (V.length xs - 1) xs == V.init xs
        , testProperty "sans all" $ \(xs :: V.Vector Int) ->
            let is = [0 .. V.length xs - 1] :: [Int]
            in V.null (foldr sans xs is)
        ]
    , testGroup
        "pickIndices"
        [ testCase "1" $ 30820 @=? pickIndices [1, 1] [27, 32, 9, 28]
        , testCase "2" $ 22776 @=? pickIndices [2, 1] [27, 32, 9, 28]
        ]
    , testGroup
        "indices"
        [ testCase "1" $ [[1, 1], [2, 1]] @=? indices 4
        , testProperty "n!" $ forAll (choose (3, 10)) $ \n ->
            n > 2 ==> length (indices n) == product @[] [1 .. n - 2]
        ]
    , testGroup
        "scores"
        [ testCase "1" $ [([1, 1], 30820), ([2, 1], 22776)] @=?
          scores [27, 32, 9, 28]
        ]
    ]

