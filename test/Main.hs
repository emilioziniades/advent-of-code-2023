import Test.Tasty

import Day01Test
import Day02Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ day01Tests
        , day02Tests
        ]
