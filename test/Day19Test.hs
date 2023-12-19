module Day19Test (day19Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day19
import Util.Fetch

day19Tests :: TestTree
day19Tests =
    testGroup
        "Day 19"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- addAcceptedRatingNumbers "example/day_19.txt"
                actual @?= 19114
            , testCase "Input" $ do
                getInput 19
                actual <- addAcceptedRatingNumbers "input/day_19.txt"
                actual @?= 456651
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- countRatingCombinations "example/day_19.txt"
                actual @?= 167409079868000
            , testCase "Input" $ do
                getInput 19
                actual <- countRatingCombinations "input/day_19.txt"
                actual @?= 131899818301477
            ]
        ]
