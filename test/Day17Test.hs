module Day17Test (day17Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day17

day17Tests :: TestTree
day17Tests =
    testGroup
        "Day 17"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- minimizeHeatLoss "example/day_17.txt"
                actual @?= 102
            ]
        ]
