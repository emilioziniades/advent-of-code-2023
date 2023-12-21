module Day21Test (day21Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day21
import Util.Fetch

day21Tests :: TestTree
day21Tests =
    testGroup
        "Day 21"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- countSteps 6 "example/day_21.txt"
                actual @?= 16
            , testCase "Input" $ do
                getInput 21
                actual <- countSteps 64 "input/day_21.txt"
                actual @?= 3637
            ]
        ]
