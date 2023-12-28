module Day20Test (day20Tests) where

import Day20
import Test.Tasty
import Test.Tasty.HUnit
import Util.Fetch

day20Tests :: TestTree
day20Tests =
    testGroup
        "Day 20"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- measurePulses "example/day_20.txt"
                actual @?= 32000000
            , testCase "Another Example" $ do
                actual <- measurePulses "example/day_20_part1.txt"
                actual @?= 11687500
            , testCase "Input" $ do
                getInput 20
                actual <- measurePulses "input/day_20.txt"
                actual @?= 825167435
            ]
        , testGroup
            "Part 2"
            [ testCase "Input" $ do
                getInput 20
                actual <- broadcastLowRx "input/day_20.txt"
                actual @?= 0
            ]
        ]
