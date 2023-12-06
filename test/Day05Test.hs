module Day05Test (day05Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day05
import Fetch

day05Tests :: TestTree
day05Tests =
    testGroup
        "Day 5"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- lowestLocationNumber "example/day_05.txt"
                actual @?= 35
            , testCase "Input" $ do
                getInput 5
                actual <- lowestLocationNumber "input/day_05.txt"
                actual @?= 261668924
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- lowestLocationNumberRanges "example/day_05.txt"
                actual @?= 46
            ]
        ]
