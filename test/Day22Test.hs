module Day22Test (day22Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day22
import Util.Fetch

day22Tests :: TestTree
day22Tests =
    testGroup
        "Day 22"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- countDisintegratable "example/day_22.txt"
                actual @?= 5
            , testCase "Input" $ do
                getInput 22
                actual <- countDisintegratable "input/day_22.txt"
                actual @?= 5
            ]
        ]
