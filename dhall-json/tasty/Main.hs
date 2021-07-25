{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void   (Void)
import Test.Tasty  (TestTree)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Text
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
import qualified Test.Tasty.Silver

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
        , Test.Tasty.testGroup "Errors"
            [ testJSONToDhallErrorMessage "./tasty/data/error/mismatchMessage0" defaultConversion
            , testJSONToDhallErrorMessage "./tasty/data/error/mismatchMessage1" defaultConversion
            , testJSONToDhallErrorMessage "./tasty/data/error/mismatchMessage2" defaultConversion
            , testJSONToDhallErrorMessage "./tasty/data/error/unhandledKeys" strictRecs
            , testJSONToDhallErrorMessage "./tasty/data/error/missingKey" strictRecs
            ]
        , Test.Tasty.testGroup "Nesting"
            [ testDhallToJSON "./tasty/data/nesting0"
            , testDhallToJSON "./tasty/data/nesting1"
            , testDhallToJSON "./tasty/data/nesting2"
            , testDhallToJSON "./tasty/data/nesting3"
            , testDhallToJSON "./tasty/data/nestingLegacy0"
            , testDhallToJSON "./tasty/data/nestingLegacy1"
            , testDhallToJSON "./tasty/data/time"
            ]
        , Test.Tasty.testGroup "Union keys"
            [ testJSONToDhall "./tasty/data/unionKeys"
            , testDhallToJSON "./tasty/data/unionKeys"
            ]
        ]
    where
        defaultConversion = JSONToDhall.defaultConversion
        omissibleLists = defaultConversion{JSONToDhall.omissibleLists = True}
        strictRecs = defaultConversion{JSONToDhall.strictRecs = True}

testDhallToJSON :: String -> TestTree
testDhallToJSON prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix <> ".dhall"
    let outputFile = prefix <> ".json"

    text <- Data.Text.IO.readFile inputFile

    parsedExpression <-
        Core.throws (Dhall.Parser.exprFromText inputFile text)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let convertedExpression =
            Dhall.JSON.convertToHomogeneousMaps Dhall.JSON.defaultConversion resolvedExpression

    actualValue <-
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

    value <-
        case Aeson.eitherDecode bytes of
            Left string -> fail string
            Right value -> return value

    schema <-
        if infer
            then return (JSONToDhall.schemaToDhallType (JSONToDhall.inferSchema value))
            else let schemaFile = prefix <> "Schema.dhall" in loadSchemaFromFile schemaFile

    _ <- Core.throws (Dhall.TypeCheck.typeOf schema)

    actualExpression <-
        Core.throws (JSONToDhall.dhallFromJSON conv schema value)

    outputText <- Data.Text.IO.readFile outputFile

    parsedExpression <-
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

loadSchemaFromFile :: FilePath -> IO (Core.Expr Dhall.Parser.Src Void)
loadSchemaFromFile schemaFile = do
    schemaText <- Data.Text.IO.readFile schemaFile

    parsedSchema <- Core.throws (Dhall.Parser.exprFromText schemaFile schemaText)

    Dhall.Import.load parsedSchema

testJSONToDhallErrorMessage :: String -> JSONToDhall.Conversion -> TestTree
testJSONToDhallErrorMessage prefix conv =
    Test.Tasty.Silver.goldenVsAction prefix goldenFile action converter
    where
        goldenFile = prefix <> ".golden"
        schemaFile = prefix <> "Schema.dhall"
        inputFile  = prefix <> ".json"

        action = do
            bytes <- Data.ByteString.Lazy.readFile inputFile

            value <-
                case Aeson.eitherDecode bytes of
                    Left string -> fail string
                    Right value -> return value

            schema <- loadSchemaFromFile schemaFile

            _ <- Core.throws (Dhall.TypeCheck.typeOf schema)

            case JSONToDhall.dhallFromJSON conv schema value of
                Right _ -> fail $ prefix <> " should fail"
                Left compileError ->
                    return (Data.Text.pack $ show compileError)

        converter = id
