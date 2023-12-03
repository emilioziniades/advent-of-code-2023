module Day03Test (day03Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day03
import Fetch (getInput)

day03Tests :: TestTree
day03Tests =
    testGroup
        "Day 3"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- sumPartNumbers "example/Day03.txt"
                actual @?= 4361
            ]
        , testGroup
            "Part 1"
            [ testCase "Input" $ do
                getInput 3
                actual <- sumPartNumbers "input/Day03.txt"
                actual @?= 554003
            ]
        ]
