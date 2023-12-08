module Day08Test (day08Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day08
import Util.Fetch

day08Tests :: TestTree
day08Tests =
    testGroup
        "Day 8"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- countSteps "example/day_08.txt"
                actual @?= 2
            , testCase "Another Example" $ do
                actual <- countSteps "example/day_08_part1.txt"
                actual @?= 6
            , testCase "Input" $ do
                getInput 8
                actual <- countSteps "input/day_08.txt"
                actual @?= 18157
            ]
        ]
