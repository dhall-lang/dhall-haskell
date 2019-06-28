module Main where

import System.FilePath ((</>))
import Test.Tasty      (TestTree)

import qualified Dhall.Test.Dhall
import qualified Dhall.Test.Diff
import qualified Dhall.Test.Format
import qualified Dhall.Test.Import
import qualified Dhall.Test.Lint
import qualified Dhall.Test.Normalization
import qualified Dhall.Test.Parser
import qualified Dhall.Test.QuickCheck
import qualified Dhall.Test.Regression
import qualified Dhall.Test.Tutorial
import qualified Dhall.Test.TypeCheck
import qualified Dhall.Test.TypeInference
import qualified GHC.IO.Encoding
import qualified System.Directory
import qualified System.Environment
import qualified System.IO
import qualified Test.Tasty

getAllTests :: IO TestTree
getAllTests = do
    normalizationTests <- Dhall.Test.Normalization.getTests

    parsingTests <- Dhall.Test.Parser.getTests

    formattingTests <- Dhall.Test.Format.getTests

    typecheckingTests <- Dhall.Test.TypeCheck.getTests

    typeinferenceTests <- Dhall.Test.TypeInference.getTests

    importingTests <- Dhall.Test.Import.getTests

    lintTests <- Dhall.Test.Lint.getTests

    diffTests <- Dhall.Test.Diff.getTests

    let testTree =
            Test.Tasty.testGroup "Dhall Tests"
                [ normalizationTests
                , parsingTests
                , importingTests
                , typecheckingTests
                , typeinferenceTests
                , formattingTests
                , lintTests
                , diffTests
                , Dhall.Test.Regression.tests
                , Dhall.Test.Tutorial.tests
                , Dhall.Test.QuickCheck.tests
                , Dhall.Test.Dhall.tests
                ]

    return testTree

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    pwd <- System.Directory.getCurrentDirectory

    System.Environment.setEnv "XDG_CACHE_HOME" (pwd </> ".cache")

    allTests <- getAllTests

    Test.Tasty.defaultMain allTests
