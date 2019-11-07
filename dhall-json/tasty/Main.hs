{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Monoid ((<>))
import Dhall.JSON (Conversion(..))
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
        , testJSONToDhall "./tasty/data/emptyAlternative"
        , testJSONToDhall "./tasty/data/emptyObject"
        , testJSONToDhall "./tasty/data/emptyList"
        , testJSONToDhall "./tasty/data/emptyObjectStrongType"
        , testJSONToDhall "./tasty/data/emptyListStrongType"
        , testCustomConversionJSONToDhall omissibleLists "./tasty/data/missingList"
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

testCustomConversionJSONToDhall :: JSONToDhall.Conversion -> String -> TestTree
testCustomConversionJSONToDhall conv prefix =
  Test.Tasty.HUnit.testCase prefix $ do
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
        Core.throws (JSONToDhall.dhallFromJSON conv schema value)

    outputText <- Data.Text.IO.readFile outputFile

    parsedExpression <- do
        Core.throws (Dhall.Parser.exprFromText outputFile outputText)

    resolvedExpression <- Dhall.Import.load parsedExpression

    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)

    let expectedExpression = Core.normalize resolvedExpression

    let message =
            "Conversion to Dhall did not generate the expected output"

    Test.Tasty.HUnit.assertEqual message expectedExpression actualExpression

testJSONToDhall :: String -> TestTree
testJSONToDhall = testCustomConversionJSONToDhall JSONToDhall.defaultConversion
