module Day20Test (day20Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day20

day20Tests :: TestTree
day20Tests =
    testGroup
        "Day 20"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- measurePulses "example/day_20.txt"
                actual @?= 20114
            ]
        ]
