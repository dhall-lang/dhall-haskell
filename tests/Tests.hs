module Main where

import Normalization (normalizationTests)
import Examples (exampleTests)
import Regression (regressionTests)
import Tutorial (tutorialTests)
import Test.Tasty

allTests :: TestTree
allTests =
    testGroup "Dhall Tests"
        [ normalizationTests
        , exampleTests
        , tutorialTests
        , regressionTests
        ]

main :: IO ()
main = defaultMain allTests
