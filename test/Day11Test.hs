module Day11Test (day11Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day11
import Util.Fetch (getInput)

day11Tests :: TestTree
day11Tests =
    testGroup
        "Day 11"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- sumPaths "example/day_11.txt"
                actual @?= 374
            , testCase "Input" $ do
                getInput 11
                actual <- sumPaths "input/day_11.txt"
                actual @?= 9724940
            ]
        ]
