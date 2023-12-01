import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Fetch

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ testGroup
            "Day 1"
            [ testGroup
                "Part 1"
                [ testCase "Example" $ do
                    actual <- sumCalibrationValues calibrationValue "example/Day01.txt"
                    actual @?= 142
                , testCase "Input" $ do
                    getInput 1
                    actual <- sumCalibrationValues calibrationValue "input/Day01.txt"
                    actual @?= 54390
                ]
            , testGroup
                "Part 2"
                [ testCase "Example" $ do
                    actual <- sumCalibrationValues calibrationValueLiteral "example/Day01_Part2.txt"
                    actual @?= 281
                , testCase "Input" $ do
                    getInput 1
                    actual <- sumCalibrationValues calibrationValueLiteral "input/Day01.txt"
                    actual @?= 54277
                ]
            ]
        ]
