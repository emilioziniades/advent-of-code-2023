module Day09Test (day09Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day09
import Util.Fetch

day09Tests :: TestTree
day09Tests =
    testGroup
        "Day 9"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- extrapolateValues "example/day_09.txt"
                actual @?= 114
            , testCase "Input" $ do
                getInput 9
                actual <- extrapolateValues "input/day_09.txt"
                actual @?= 0
            ]
        ]
