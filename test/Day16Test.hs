module Day16Test (day16Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day16

day16Tests :: TestTree
day16Tests =
    testGroup
        "Day 16"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- energizedSquares "example/day_16.txt"
                actual @?= 1320
            ]
        ]
