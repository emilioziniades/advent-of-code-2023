module Day14Test (day14Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day14
import Util.Fetch (getInput)

day14Tests :: TestTree
day14Tests =
    testGroup
        "Day 14"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- measureLoad "example/day_14.txt"
                actual @?= 136
            , testCase "Input" $ do
                getInput 14
                actual <- measureLoad "input/day_14.txt"
                actual @?= 103333
            ]
        ]
