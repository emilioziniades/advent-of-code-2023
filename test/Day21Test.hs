module Day21Test (day21Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day21

day21Tests :: TestTree
day21Tests =
    testGroup
        "Day 21"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- countSteps 6 "example/day_21.txt"
                actual @?= 16
            ]
        ]
