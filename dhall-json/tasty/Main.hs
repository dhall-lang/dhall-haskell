{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Monoid ((<>))
import Test.Tasty (TestTree)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Dhall.Core           as Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.JSONToDhall    as JSONToDhall
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-json"
        [ testDhallToJSON "./tasty/data/issue48"
        , testDhallToJSON "./tasty/data/emptyObjectStrongType"
        , testDhallToJSON "./tasty/data/toArbitraryJSON_12_0_0"
        , testDhallToJSON "./tasty/data/toArbitraryJSON_13_0_0"
        , testJSONToDhall "./tasty/data/emptyAlternative"
        , testJSONToDhall "./tasty/data/emptyObject"
        , testJSONToDhall "./tasty/data/emptyList"
        , testJSONToDhall "./tasty/data/emptyObjectStrongType"
        , testJSONToDhall "./tasty/data/emptyListStrongType"
        , testJSONToDhall "./tasty/data/fromArbitraryJSON_12_0_0"
        , testJSONToDhall "./tasty/data/fromArbitraryJSON_13_0_0"
        , inferJSONToDhall "./tasty/data/potpourri"
        , testCustomConversionJSONToDhall False omissibleLists "./tasty/data/missingList"
        , Test.Tasty.testGroup "Nesting"
            [ testDhallToJSON "./tasty/data/nesting0"
            , testDhallToJSON "./tasty/data/nesting1"
            , testDhallToJSON "./tasty/data/nesting2"
            , testDhallToJSON "./tasty/data/nesting3"
            , testDhallToJSON "./tasty/data/nestingLegacy0"
            , testDhallToJSON "./tasty/data/nestingLegacy1"
            ]
        , Test.Tasty.testGroup "Union keys"
            [ testJSONToDhall "./tasty/data/unionKeys"
            , testDhallToJSON "./tasty/data/unionKeys"
            ]
        ]
    where omissibleLists = JSONToDhall.defaultConversion{JSONToDhall.omissibleLists = True}

testDhallToJSON :: String -> TestTree
testDhallToJSON prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix <> ".dhall"
    let outputFile = prefix <> ".json"

    text <- Data.Text.IO.readFile inputFile

    parsedExpression <- do
        Core.throws (Dhall.Parser.exprFromText inputFile text)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let convertedExpression =
            Dhall.JSON.convertToHomogeneousMaps Dhall.JSON.defaultConversion resolvedExpression

    actualValue <- do
        Core.throws (Dhall.JSON.dhallToJSON convertedExpression)

    bytes <- Data.ByteString.Lazy.readFile outputFile

    expectedValue <- case Aeson.eitherDecode bytes of
        Left  string        -> fail string
        Right expectedValue -> return expectedValue

    let message = "Conversion to JSON did not generate the expected output"

    Test.Tasty.HUnit.assertEqual message expectedValue actualValue

testCustomConversionJSONToDhall
    :: Bool -> JSONToDhall.Conversion -> String -> TestTree
testCustomConversionJSONToDhall infer conv prefix =
  Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix <> ".json"
    let outputFile = prefix <> ".dhall"

    bytes <- Data.ByteString.Lazy.readFile inputFile

    value <- do
        case Aeson.eitherDecode bytes of
            Left string -> fail string
            Right value -> return value

    schema <- do
        if infer
            then do
                return (JSONToDhall.schemaToDhallType (JSONToDhall.inferSchema value))
            else do
                let schemaFile = prefix <> "Schema.dhall"

                schemaText <- Data.Text.IO.readFile schemaFile

                parsedSchema <- Core.throws (Dhall.Parser.exprFromText schemaFile schemaText)

                Dhall.Import.load parsedSchema

    _ <- Core.throws (Dhall.TypeCheck.typeOf schema)

    actualExpression <- do
        Core.throws (JSONToDhall.dhallFromJSON conv schema value)

    outputText <- Data.Text.IO.readFile outputFile

    parsedExpression <- do
        Core.throws (Dhall.Parser.exprFromText outputFile outputText)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let expectedExpression = Core.denote resolvedExpression

    let message =
            "Conversion to Dhall did not generate the expected output"

    Test.Tasty.HUnit.assertEqual message expectedExpression actualExpression

testJSONToDhall :: String -> TestTree
testJSONToDhall =
    testCustomConversionJSONToDhall False JSONToDhall.defaultConversion

inferJSONToDhall :: String -> TestTree
inferJSONToDhall =
    testCustomConversionJSONToDhall True JSONToDhall.defaultConversion
