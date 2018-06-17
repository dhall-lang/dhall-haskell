module Main where

import Normalization (normalizationTests)
import Optimization (optimizationTests)
import Parser (parserTests)
import Regression (regressionTests)
import Tutorial (tutorialTests)
import TypeCheck (typecheckTests)
import Format (formatTests)
import Test.Tasty

allTests :: TestTree
allTests =
    testGroup "Dhall Tests"
        [ normalizationTests
        , optimizationTests
        , parserTests
        , regressionTests
        , tutorialTests
        , formatTests
        , typecheckTests
        ]

main :: IO ()
main = defaultMain allTests
