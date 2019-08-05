{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Monoid ((<>))
import Dhall.JSON (Conversion(..))
import Test.Tasty (TestTree)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Dhall.Core           as Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.JSONToDhall    as JSONToDhall
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Dhall.Yaml
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-json"
        [ testDhallToJSON "./tasty/data/issue48"
        , testDhallToYaml
            Dhall.Yaml.defaultOptions
            "./tasty/data/normal"
        , testDhallToYaml
            (Dhall.Yaml.defaultOptions { Dhall.Yaml.quoted = True })
            "./tasty/data/quoted"
        , testJSONToDhall "./tasty/data/emptyAlternative"
        , Test.Tasty.testGroup "Nesting"
            [ testDhallToJSON "./tasty/data/nesting0"
            , testDhallToJSON "./tasty/data/nesting1"
            , testDhallToJSON "./tasty/data/nesting2"
            , testDhallToJSON "./tasty/data/nestingLegacy0"
            , testDhallToJSON "./tasty/data/nestingLegacy1"
            ]
        , Test.Tasty.testGroup "Union keys"
            [ testJSONToDhall "./tasty/data/unionKeys"
            , testDhallToJSON "./tasty/data/unionKeys"
            ]
        ]

testDhallToJSON :: String -> TestTree
testDhallToJSON prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix <> ".dhall"
    let outputFile = prefix <> ".json"

    text <- Data.Text.IO.readFile inputFile

    parsedExpression <- do
        Core.throws (Dhall.Parser.exprFromText inputFile text)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let mapKey     = "mapKey"
    let mapValue   = "mapValue"
    let conversion = Conversion {..}

    let convertedExpression =
            Dhall.JSON.convertToHomogeneousMaps conversion resolvedExpression

    actualValue <- do
        Core.throws (Dhall.JSON.dhallToJSON convertedExpression)

    bytes <- Data.ByteString.Lazy.readFile outputFile

    expectedValue <- case Aeson.eitherDecode bytes of
        Left  string        -> fail string
        Right expectedValue -> return expectedValue

    let message = "Conversion to JSON did not generate the expected output"

    Test.Tasty.HUnit.assertEqual message expectedValue actualValue

testJSONToDhall :: String -> TestTree
testJSONToDhall prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix <> ".json"
    let schemaFile = prefix <> "Schema.dhall"
    let outputFile = prefix <> ".dhall"

    bytes <- Data.ByteString.Lazy.readFile inputFile

    value <- do
        case Aeson.eitherDecode bytes of
            Left string -> fail string
            Right value -> return value

    schemaText <- Data.Text.IO.readFile schemaFile

    parsedSchema <- Core.throws (Dhall.Parser.exprFromText schemaFile schemaText)

    schema <- Dhall.Import.load parsedSchema

    _ <- Core.throws (Dhall.TypeCheck.typeOf schema)

    actualExpression <- do
        Core.throws (JSONToDhall.dhallFromJSON JSONToDhall.defaultConversion schema value)

    outputText <- Data.Text.IO.readFile outputFile

    parsedExpression <- do
        Core.throws (Dhall.Parser.exprFromText outputFile outputText)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let expectedExpression = Core.normalize resolvedExpression

    let message =
            "Conversion to Dhall did not generate the expected output"

    Test.Tasty.HUnit.assertEqual message expectedExpression actualExpression

testDhallToYaml :: Dhall.Yaml.Options -> String -> TestTree
testDhallToYaml options prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix <> ".dhall"
    let outputFile = prefix <> ".yaml"

    text <- Data.Text.IO.readFile inputFile

    actualValue <- do
        Dhall.Yaml.dhallToYaml options (Just inputFile) text

    expectedValue <- Data.ByteString.readFile outputFile

    let message = "Conversion to YAML did not generate the expected output"

    Test.Tasty.HUnit.assertEqual message expectedValue actualValue
