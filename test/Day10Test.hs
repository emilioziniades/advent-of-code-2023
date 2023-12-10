module Day10Test (day10Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day10
import Util.Fetch

day10Tests :: TestTree
day10Tests =
    testGroup
        "Day 10"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- farthestLoopPoint "example/day_10.txt"
                actual @?= 4
            , testCase "Another Example" $ do
                actual <- farthestLoopPoint "example/day_10_part1.txt"
                actual @?= 8
            , testCase "Input" $ do
                getInput 10
                actual <- farthestLoopPoint "input/day_10.txt"
                actual @?= 6882
            ]
        ]
