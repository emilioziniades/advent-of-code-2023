module Day05Test (day05Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day05

day05Tests :: TestTree
day05Tests =
    testGroup
        "Day 5"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- lowestLocationNumber "example/day_05.txt"
                actual @?= 35
            ]
        ]
