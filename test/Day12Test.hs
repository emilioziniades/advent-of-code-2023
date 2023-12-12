module Day12Test (day12Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day12
import Util.Fetch

day12Tests :: TestTree
day12Tests =
    testGroup
        "Day 12"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- springArrangements "example/day_12.txt"
                actual @?= 21
            , testCase "Input" $ do
                getInput 12
                actual <- springArrangements "input/day_12.txt"
                actual @?= 0
            ]
        ]
