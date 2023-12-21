module Day21Test (day21Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day21
import Util.Fetch

day21Tests :: TestTree
day21Tests =
    testGroup
        "Day 21"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- countSteps 6 "example/day_21.txt"
                actual @?= 16
            , testCase "Input" $ do
                getInput 21
                actual <- countSteps 64 "input/day_21.txt"
                actual @?= 3637
            ]
        , testGroup
            "Part 2"
            [ testCase "Example 1" $ do
                actual <- countMoreSteps 6 "example/day_21.txt"
                actual @?= 16
            , testCase "Example 2" $ do
                actual <- countMoreSteps 10 "example/day_21.txt"
                actual @?= 50
            , testCase "Example 3" $ do
                actual <- countMoreSteps 50 "example/day_21.txt"
                actual @?= 1594
            , testCase "Example 4" $ do
                actual <- countMoreSteps 100 "example/day_21.txt"
                actual @?= 6536
            , testCase "Example 5" $ do
                actual <- countMoreSteps 500 "example/day_21.txt"
                actual @?= 167004
            , testCase "Example 6" $ do
                actual <- countMoreSteps 1000 "example/day_21.txt"
                actual @?= 668697
            , testCase "Example 7" $ do
                actual <- countMoreSteps 5000 "example/day_21.txt"
                actual @?= 16733044
            ]
        ]
