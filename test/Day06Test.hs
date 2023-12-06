module Day06Test (day06Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day06
import Util.Fetch

day06Tests :: TestTree
day06Tests =
    testGroup
        "Day 6"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- recordBreakingProduct "example/day_06.txt"
                actual @?= 288
            , testCase "Input" $ do
                getInput 6
                actual <- recordBreakingProduct "input/day_06.txt"
                actual @?= 3316275
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- recordBreakingRace "example/day_06.txt"
                actual @?= 71503
            , testCase "Input" $ do
                actual <- recordBreakingRace "input/day_06.txt"
                actual @?= 27102791
            ]
        ]
