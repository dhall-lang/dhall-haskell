{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dhall.JSON.Yaml (Options (..))
import Test.Tasty      (TestTree)

import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Dhall.Core
import qualified Dhall.JSON.Yaml
import qualified Dhall.Yaml
import qualified Dhall.YamlToDhall          as YamlToDhall
import qualified GHC.IO.Encoding
import qualified Test.Tasty
import qualified Test.Tasty.ExpectedFailure as Tasty.ExpectedFailure
import qualified Test.Tasty.HUnit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Test.Tasty.defaultMain testTree

data TestScope
    = SkipAesonYaml String -- ^ To skip aeson-yaml tests. "String" is to let us know why are we skipping it
    | SkipHsYAML String -- ^ As above, but for HsYAML
    | TestBoth -- ^ Tests both integrations

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-yaml"
        [ testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/normal" $
            SkipAesonYaml "aeson-yaml uses yaml 1.2 so it doesn't quotes yaml 1.1 boolean strings"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/normal-aeson" $
            SkipHsYAML "HsYAML integration let us quotes boolean strings for backwards compatibility with yaml 1.1"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/special"
            TestBoth
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/emptyList"
            TestBoth
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/emptyMap"
            TestBoth
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions{ quoted = True }
            "./tasty/data/quoted"
            TestBoth
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/boolean-quotes" $
            SkipAesonYaml "this test is just for HsYAML integration"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions{ documents = True }
            "./tasty/data/single-document"
            TestBoth
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions{ documents = True }
            "./tasty/data/single-document-bare"
            TestBoth
        , testYamlToDhall
            "./tasty/data/mergify"
        ]

testDhallToYaml :: Options -> String -> TestScope -> TestTree
testDhallToYaml options prefix testScope =
    Test.Tasty.testGroup prefix (
        case testScope of
            SkipAesonYaml _ -> [hsYamlTest, Tasty.ExpectedFailure.expectFail hsAesonYamlTest]
            SkipHsYAML _ -> [hsAesonYamlTest, Tasty.ExpectedFailure.expectFail hsYamlTest]
            _ -> [hsYamlTest, hsAesonYamlTest]
    )
  where
    hsYamlTest = testCase Dhall.Yaml.dhallToYaml "HsYAML"
    hsAesonYamlTest = testCase Dhall.JSON.Yaml.dhallToYaml "aeson-yaml"
    testCase dhallToYaml s = Test.Tasty.HUnit.testCase s $ do
        let inputFile = prefix <> ".dhall"
        let outputFile = prefix <> ".yaml"

        text <- Data.Text.IO.readFile inputFile

        actualValue <- dhallToYaml options (Just inputFile) text

        expectedValue <- Data.ByteString.readFile outputFile

        let message = "Conversion to YAML did not generate the expected output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue

testYamlToDhall :: String -> TestTree
testYamlToDhall prefix =
    Test.Tasty.HUnit.testCase prefix $ do
        let inputFile = prefix <> ".yaml"
        let outputFile = prefix <> ".dhall"

        bytes <- Data.ByteString.readFile inputFile

        expression <- YamlToDhall.dhallFromYaml (YamlToDhall.defaultOptions Nothing) bytes

        let actualValue = Dhall.Core.pretty expression <> "\n"

        expectedValue <- Data.Text.IO.readFile outputFile

        let message =
                "Conversion from YAML did not generate the expected output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue
