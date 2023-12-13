module Day13Test (day13Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day13
import Util.Fetch

day13Tests :: TestTree
day13Tests =
    testGroup
        "Day 13"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- findSymmetries "example/day_13.txt"
                actual @?= 405
            , testCase "Input" $ do
                getInput 13
                actual <- findSymmetries "input/day_13.txt"
                actual @?= 35538
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- findSingleAsymmetries "example/day_13.txt"
                actual @?= 400
            , testCase "Input" $ do
                getInput 13
                actual <- findSingleAsymmetries "input/day_13.txt"
                actual @?= 30442
            ]
        ]
