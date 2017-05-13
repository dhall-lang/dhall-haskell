module Main where

import Normalization (normalizationTests)
import Examples (exampleTests)
import Test.Tasty

allTests :: TestTree
allTests =
    testGroup "Dhall Tests"
        [ normalizationTests
        , exampleTests
        ]

main :: IO ()
main = defaultMain allTests
