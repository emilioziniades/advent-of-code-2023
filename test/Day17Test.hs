module Day17Test (day17Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day17
import Util.Fetch

day17Tests :: TestTree
day17Tests =
    testGroup
        "Day 17"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- minimizeHeatLoss "example/day_17.txt"
                actual @?= 102
            , testCase "Input" $ do
                getInput 17
                actual <- minimizeHeatLoss "input/day_17.txt"
                actual @?= 886
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- minimizeHeatLossUltra "example/day_17.txt"
                actual @?= 94
            , testCase "Input" $ do
                getInput 17
                actual <- minimizeHeatLossUltra "input/day_17.txt"
                actual @?= 1055
            ]
        ]
