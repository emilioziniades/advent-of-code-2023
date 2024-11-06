module Day23Test (day23Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day23 (findLongestHike)
import Util.Fetch (getInput)

day23Tests :: TestTree
day23Tests =
    testGroup
        "Day 23"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                file <- readFile "example/day_23.txt"
                let actual = findLongestHike file
                actual @?= 94
            , testCase "Input" $ do
                getInput 23
                file <- readFile "input/day_23.txt"
                let actual = findLongestHike file
                actual @?= 2402
            ]
        ]
