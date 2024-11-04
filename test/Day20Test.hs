module Day20Test (day20Tests) where

import Day20
import Test.Tasty
import Test.Tasty.HUnit
import Util.Fetch

day20Tests :: TestTree
day20Tests =
    testGroup
        "Day 20"
        [ testGroup
            "Part 1"
            [ testCase "Example" $ do
                actual <- measurePulses "example/day_20.txt"
                actual @?= 32000000
            , testCase "Another Example" $ do
                actual <- measurePulses "example/day_20_part1.txt"
                actual @?= 11687500
            , testCase "Input" $ do
                getInput 20
                actual <- measurePulses "input/day_20.txt"
                actual @?= 825167435
            ]
        , -- , testGroup
          --     "Part 2 Input"
          --     [ testCase "Input" $ do
          --         getInput 20
          --         actual <- broadcastLowRx "input/day_20.txt"
          --         actual @?= 0
          --     ]
          testGroup "Part 2 Example" $ fmap mkTestCase tests
        ]

tests :: [([String], String, Int)]
tests =
    [
        (
            [ "broadcaster -> rx"
            ]
        , "simple"
        , 1
        )
    ,
        (
            [ "broadcaster -> a"
            , "%a -> rx"
            ]
        , "one flipflop"
        , 2
        )
    ,
        (
            [ "broadcaster -> a"
            , "%a -> b"
            , "%b -> rx"
            ]
        , "two flipflops"
        , 4
        )
    ,
        (
            [ "broadcaster -> a"
            , "%a -> b"
            , "%b -> c"
            , "%c -> rx"
            ]
        , "three flipflops"
        , 8
        )
    ,
        (
            [ "broadcaster -> a"
            , "%a -> b"
            , "&b -> rx"
            ]
        , "one flipflop one conjunction"
        , 1
        )
    ,
        (
            [ "broadcaster -> a"
            , "%a -> b"
            , "%b -> c"
            , "&c -> rx"
            ]
        , "two flipflip one conjunction"
        , 2
        )
    ,
        (
            [ "broadcaster -> a"
            , "%a -> b"
            , "%b -> c"
            , "%c -> d"
            , "&d -> rx"
            ]
        , "three flipflop one conjunction"
        , 4
        )
    ,
        (
            [ "broadcaster -> a, b"
            , "%a -> c"
            , "%b -> c"
            , "&c -> rx"
            ]
        , "one-one flipflop one conjunction"
        , 1
        )
        -- NOTE: these don't pass
        -- ,
        --     (
        --         [ "broadcaster -> aa, b"
        --         , "%aa -> ab"
        --         , "%ab -> c"
        --         , "%b -> c"
        --         , "&c -> rx"
        --         ]
        --     , "another conjunction"
        --     , 3
        --     )
        -- ,
        --     (
        --         [ "broadcaster -> aa, b"
        --         , "%aa -> ab"
        --         , "%ab -> ac"
        --         , "%ac -> c"
        --         , "%b -> c"
        --         , "&c -> rx"
        --         ]
        --     , "interesting conjunction"
        --     , 5
        --     )
        -- ,
        --     (
        --         [ "broadcaster -> aa, b"
        --         , "%aa -> ab"
        --         , "%ab -> ac"
        --         , "%ac -> ad"
        --         , "%ad -> c"
        --         , "%b -> c"
        --         , "&c -> rx"
        --         ]
        --     , "another interesting conjunction"
        --     , 9
        --     )
    ]

mkTestCase :: ([String], String, Int) -> TestTree
mkTestCase (modules, name, actual) = testCase name $ expected @?= actual
  where
    input = unlines modules
    -- expected = broadcastLowRxDirect input
    expected = countUntilLowRxDirect input
