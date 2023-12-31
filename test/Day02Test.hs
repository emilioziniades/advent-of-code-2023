module Day02Test (day02Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day02
import Util.Fetch

day02Tests :: TestTree
day02Tests =
    testGroup
        "Day 2"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- sumPossibleGameIds "example/day_02.txt"
                actual @?= 8
            , testCase "Input" $ do
                getInput 2
                actual <- sumPossibleGameIds "input/day_02.txt"
                actual @?= 3035
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- sumCubePower "example/day_02.txt"
                actual @?= 2286
            , testCase "Input" $ do
                getInput 2
                actual <- sumCubePower "input/day_02.txt"
                actual @?= 66027
            ]
        ]
