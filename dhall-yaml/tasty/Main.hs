{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Exception (SomeException, try)
import Data.Text         (Text)
import Dhall.JSON.Yaml   (Options (..))
import Test.Tasty        (TestTree)

import qualified Data.ByteString
import qualified Data.Text          as Text
import qualified Data.Text.Encoding
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
    | OnlyDhallYaml -- ^ Rules of this package. aeson-yaml is a separate renderer.

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
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions{ preserveHeader = True }
            "./tasty/data/preserve-header"
            TestBoth
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/quoting-edges"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/plain-indicators"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/null"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/numbers"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/special-doubles"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/key-order"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/nested"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions{ documents = True }
            "./tasty/data/two-documents"
            OnlyDhallYaml
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions{ noEdit = True }
            "./tasty/data/generated-comment"
            OnlyDhallYaml
        , testYamlToDhall "./tasty/data/decode-bools"
        , testYamlToDhall "./tasty/data/decode-bool-words"
        , testYamlToDhall "./tasty/data/decode-quoted-true"
        , testYamlToDhall "./tasty/data/decode-null"
        , testYamlToDhall "./tasty/data/decode-numbers"
        , testYamlToDhall "./tasty/data/decode-empty-list"
        , testYamlToDhall "./tasty/data/decode-empty-map"
        , testYamlToDhall "./tasty/data/decode-nested"
        , testDecodeRejects "decode-tag" "!secret 123\n" "unresolved YAML scalar encountered"
        , testYamlToDhall "./tasty/data/decode-plain-inf"
        , testYamlToDhall "./tasty/data/decode-duplicate-key"
        , testYamlToDhall "./tasty/data/decode-dot-inf"
        , testYamlToDhall "./tasty/data/decode-dot-neg-inf"
        , testYamlToDhall "./tasty/data/decode-dot-nan"
        , testRoundTrip "./tasty/data/roundtrip"
        , testDecodeRejects "decode-second-document" "---\n1\n---\n2\n" "Multiple documents encountered in YAML stream"
        , testDecodeRejects "decode-non-string-key" "1: a\n" "non-String key encountered in mapping"
        ]

testDhallToYaml :: Options -> String -> TestScope -> TestTree
testDhallToYaml options prefix testScope =
    Test.Tasty.testGroup prefix (
        case testScope of
            SkipAesonYaml _ -> [hsYamlTest, Tasty.ExpectedFailure.expectFail hsAesonYamlTest]
            SkipHsYAML _ -> [hsAesonYamlTest, Tasty.ExpectedFailure.expectFail hsYamlTest]
            OnlyDhallYaml -> [hsYamlTest]
            TestBoth -> [hsYamlTest, hsAesonYamlTest]
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

-- | Encode a Dhall value and decode that YAML back to the same Dhall value.
testRoundTrip :: String -> TestTree
testRoundTrip prefix =
    Test.Tasty.HUnit.testCase prefix $ do
        let inputFile = prefix <> ".dhall"

        text <- Data.Text.IO.readFile inputFile

        yaml <- Dhall.Yaml.dhallToYaml Dhall.JSON.Yaml.defaultOptions (Just inputFile) text

        expression <- YamlToDhall.dhallFromYaml (YamlToDhall.defaultOptions Nothing) yaml

        let actualValue = Dhall.Core.pretty expression <> "\n"

        Test.Tasty.HUnit.assertEqual
            "YAML did not decode back to the same Dhall value"
            text
            actualValue

-- | The YAML 1.2 core schema rejects this input. @needle@ is the stable part of the error.
testDecodeRejects :: String -> Text -> String -> TestTree
testDecodeRejects name yaml needle =
    Test.Tasty.HUnit.testCase name $ do
        result <- try @SomeException $
            YamlToDhall.dhallFromYaml
                (YamlToDhall.defaultOptions Nothing)
                (Data.Text.Encoding.encodeUtf8 yaml)
        case result of
            Right expression ->
                Test.Tasty.HUnit.assertFailure $
                    "expected rejection, decoded "
                        <> Text.unpack (Dhall.Core.pretty expression)
            Left err ->
                let shown = show err
                in Test.Tasty.HUnit.assertBool
                    ("rejection did not mention " <> needle <> ":\n" <> shown)
                    (Text.pack needle `Text.isInfixOf` Text.pack shown)
