module Day07Test (day07Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day07
import Util.Fetch

day07Tests :: TestTree
day07Tests =
    testGroup
        "Day 7"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- totalWinnings "example/day_07.txt"
                actual @?= 6440
            , testCase "Input" $ do
                getInput 7
                actual <- totalWinnings "input/day_07.txt"
                actual @?= 249748283
            ]
        ]
