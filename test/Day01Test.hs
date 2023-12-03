module Day01Test (day01Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Fetch

day01Tests :: TestTree
day01Tests =
    testGroup
        "Day 1"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- sumCalibrationValues calibrationValue "example/day_01.txt"
                actual @?= 142
            , testCase "Input" $ do
                getInput 1
                actual <- sumCalibrationValues calibrationValue "input/day_01.txt"
                actual @?= 54390
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- sumCalibrationValues calibrationValueLiteral "example/day_01_part2.txt"
                actual @?= 281
            , testCase "Input" $ do
                getInput 1
                actual <- sumCalibrationValues calibrationValueLiteral "input/day_01.txt"
                actual @?= 54277
            ]
        ]
