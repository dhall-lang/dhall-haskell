module Main where

import Normalization (normalizationTests)
import Parser (parserTests)
import Regression (regressionTests)
import QuickCheck (quickcheckTests)
import Tutorial (tutorialTests)
import TypeCheck (typecheckTests)
import Format (formatTests)
import Import (importTests)
import Test.Tasty

allTests :: TestTree
allTests =
    testGroup "Dhall Tests"
        [ normalizationTests
        , parserTests
        , regressionTests
        , tutorialTests
        , formatTests
        , typecheckTests
        , importTests
        , quickcheckTests
        ]

main :: IO ()
main = defaultMain allTests
