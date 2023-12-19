module Day17Test (day17Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day17
import Util.Fetch

day17Tests :: TestTree
day17Tests =
    testGroup
        "Day 17"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- minimizeHeatLoss "example/day_17.txt"
                actual @?= 102
            , testCase "Input" $ do
                getInput 17
                actual <- minimizeHeatLoss "input/day_17.txt"
                -- too low: 873, 875
                actual @?= 0
            ]
        ]
