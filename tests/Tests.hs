module Main where

import Normalization (normalizationTests)
import Examples (exampleTests)
import Tutorial (tutorialTests)
import Test.Tasty

allTests :: TestTree
allTests =
    testGroup "Dhall Tests"
        [ normalizationTests
        , exampleTests
        , tutorialTests
        ]

main :: IO ()
main = defaultMain allTests
