import Test.Tasty

import Day01Test
import Day02Test
import Day03Test
import Day04Test
import Day05Test
import Day06Test
import Day07Test
import Day08Test
import Day09Test
import Day10Test
import Day12Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ day01Tests
        , day02Tests
        , day03Tests
        , day04Tests
        , day05Tests
        , day06Tests
        , day07Tests
        , day08Tests
        , day09Tests
        , day10Tests
        , day12Tests
        ]
