module Main where

import Lint (lintTests)
import Normalization (normalizationTests)
import Parser (parserTests)
import Regression (regressionTests)
import QuickCheck (quickcheckTests)
import Tutorial (tutorialTests)
import TypeCheck (typecheckTests)
import Format (formatTests)
import Import (importTests)
import System.FilePath ((</>))
import Test.Tasty

import qualified System.Directory
import qualified System.Environment

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
        , lintTests
        ]

main :: IO ()
main = do
    pwd <- System.Directory.getCurrentDirectory
    System.Environment.setEnv "XDG_CACHE_HOME" (pwd </> ".cache")
    defaultMain allTests
