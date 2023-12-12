module Day10Test (day10Tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day10
import Util.Fetch

day10Tests :: TestTree
day10Tests =
    testGroup
        "Day 10"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- farthestLoopPoint "example/day_10.txt"
                actual @?= 4
            , testCase "Another Example" $ do
                actual <- farthestLoopPoint "example/day_10_part1.txt"
                actual @?= 8
            , testCase "Input" $ do
                getInput 10
                actual <- farthestLoopPoint "input/day_10.txt"
                actual @?= 6882
            ]
        , testGroup
            "Part 2"
            [ testCase "Example" $ do
                actual <- countEnclosingLoop "example/day_10_part2.txt"
                actual @?= 4
            , testCase "Example Two" $ do
                actual <- countEnclosingLoop "example/day_10_part2_2.txt"
                actual @?= 4
            , testCase "Example Three" $ do
                actual <- countEnclosingLoop "example/day_10_part2_3.txt"
                actual @?= 8
            , testCase "Example Four" $ do
                actual <- countEnclosingLoop "example/day_10_part2_4.txt"
                actual @?= 10
            , testCase "Input" $ do
                actual <- countEnclosingLoop "input/day_10.txt"
                actual @?= 491
            ]
        ]
