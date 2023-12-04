module Day04Test (day04Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day04
import Fetch

day04Tests :: TestTree
day04Tests =
    testGroup
        "Day 4"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- scratchcardPoints "example/day_04.txt"
                actual @?= 13
            , testCase "Input" $ do
                getInput 4
                actual <- scratchcardPoints "input/day_04.txt"
                actual @?= 32609
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- scratchcardsTotal "example/day_04.txt"
                actual @?= 30
                -- , testCase "Input" $ do
                --     getInput 4
                --     actual <- scratchcardPoints "input/day_04.txt"
                --     actual @?= 32609
            ]
        ]
