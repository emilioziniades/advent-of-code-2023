module Day15Test (day15Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day15
import Util.Fetch

day15Tests :: TestTree
day15Tests =
    testGroup
        "Day 15"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- sumHashes "example/day_15.txt"
                actual @?= 1320
            , testCase "Input" $ do
                getInput 15
                actual <- sumHashes "input/day_15.txt"
                actual @?= 497373
            ]
        ]
