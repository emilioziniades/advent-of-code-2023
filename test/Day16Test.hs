module Day16Test (day16Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day16
import Util.Fetch

day16Tests :: TestTree
day16Tests =
    testGroup
        "Day 16"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- energizedSquares "example/day_16.txt"
                actual @?= 46
                -- , testCase "Input" $ do
                --     getInput 16
                --     actual <- energizedSquares "input/day_16.txt"
                --     actual @?= 0
            ]
        ]
