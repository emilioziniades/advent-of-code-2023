module Day11Test (day11Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day11
import Util.Fetch (getInput)

day11Tests :: TestTree
day11Tests =
    testGroup
        "Day 11"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- sumPaths 2 "example/day_11.txt"
                actual @?= 374
            , testCase "Input" $ do
                getInput 11
                actual <- sumPaths 2 "input/day_11.txt"
                actual @?= 9724940
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- sumPaths 10 "example/day_11.txt"
                actual @?= 1030
            , testCase "Another Example" $ do
                actual <- sumPaths 100 "example/day_11.txt"
                actual @?= 8410
            , testCase "Input" $ do
                getInput 11
                actual <- sumPaths 1000000 "input/day_11.txt"
                actual @?= 569052586852
            ]
        ]
