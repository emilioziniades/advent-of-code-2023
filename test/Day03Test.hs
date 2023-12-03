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
                actual <- sumPartNumbers "example/day_03.txt"
                actual @?= 4361
            , testCase "Input" $ do
                getInput 3
                actual <- sumPartNumbers "input/day_03.txt"
                actual @?= 554003
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- sumGearRatios "example/day_03.txt"
                actual @?= 467835
            , testCase "Input" $ do
                getInput 3
                actual <- sumGearRatios "input/day_03.txt"
                actual @?= 87263515
            ]
        ]
