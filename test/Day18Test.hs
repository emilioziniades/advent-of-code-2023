module Day18Test (day18Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day18
import Util.Fetch

day18Tests :: TestTree
day18Tests =
    testGroup
        "Day 18"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- lagoonArea "example/day_18.txt"
                actual @?= 62
            , testCase "Input" $ do
                getInput 18
                actual <- lagoonArea "input/day_18.txt"
                actual @?= 53844
            ]
        ]
