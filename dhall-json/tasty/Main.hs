{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Dhall.JSON (Conversion(..))
import Test.Tasty (TestTree)

import qualified Control.Exception    as Exception
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Core           as Core
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.JSONToDhall    as JSONToDhall
import qualified Dhall.Parser
import qualified Dhall.Yaml
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-json"
        [ issue48
        , yamlQuotedStrings
        , yaml
        , emptyAlternative
        , nesting
        , unionKeys
        ]

issue48 :: TestTree
issue48 = Test.Tasty.HUnit.testCase "Issue #48" assertion
  where
    assertion = do
        let file = "./tasty/data/issue48.dhall"

        code <- Data.Text.IO.readFile file

        parsedExpression <- case Dhall.Parser.exprFromText file code of
            Left  exception        -> Exception.throwIO exception
            Right parsedExpression -> return parsedExpression

        resolvedExpression <- Dhall.Import.load parsedExpression

        let mapKey     = "mapKey"
        let mapValue   = "mapValue"
        let conversion = Conversion {..}

        let convertedExpression =
                Dhall.JSON.convertToHomogeneousMaps conversion resolvedExpression

        actualValue <- case Dhall.JSON.dhallToJSON convertedExpression of
            Left  exception   -> Exception.throwIO exception
            Right actualValue -> return actualValue

        bytes <- Data.ByteString.Lazy.readFile "./tasty/data/issue48.json"

        expectedValue <- case Aeson.eitherDecode bytes of
            Left  string        -> fail string
            Right expectedValue -> return expectedValue

        let message =
                "Conversion to homogeneous maps did not generate the expected JSON output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue

yamlQuotedStrings :: TestTree
yamlQuotedStrings = Test.Tasty.HUnit.testCase "Yaml: quoted string style" assertion
  where
    assertion = do
        let file = "./tasty/data/yaml.dhall"

        code <- Data.Text.IO.readFile file

        let options =
              Dhall.Yaml.defaultOptions { Dhall.Yaml.quoted = True }

        actualValue <-
          Dhall.Yaml.dhallToYaml options (Data.Text.pack file) code

        bytes <- Data.ByteString.Lazy.readFile "./tasty/data/quoted.yaml"

        let expectedValue = Data.ByteString.Lazy.toStrict bytes

        let message =
              "Conversion to quoted yaml did not generate the expected output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue

yaml :: TestTree
yaml = Test.Tasty.HUnit.testCase "Yaml: normal string style" assertion
  where
    assertion = do
        let file = "./tasty/data/yaml.dhall"

        code <- Data.Text.IO.readFile file

        actualValue <-
          Dhall.Yaml.dhallToYaml Dhall.Yaml.defaultOptions (Data.Text.pack file) code

        bytes <- Data.ByteString.Lazy.readFile "./tasty/data/normal.yaml"

        let expectedValue = Data.ByteString.Lazy.toStrict bytes

        let message =
                "Conversion to normal yaml did not generate the expected output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue

emptyAlternative :: TestTree
emptyAlternative = Test.Tasty.HUnit.testCase "Empty alternative" $ do
    let schema = Core.Union [ ("Bar", Nothing), ("Foo", Nothing) ]

    let json = Aeson.String "Foo"

    let expectedResult = Core.Field schema "Foo"

    actualResult <- Core.throws (JSONToDhall.dhallFromJSON JSONToDhall.defaultConversion schema json)

    let message = "Empty alternatives were not decoded from JSON correctly"

    Test.Tasty.HUnit.assertEqual message expectedResult actualResult

nesting :: TestTree
nesting = Test.Tasty.testGroup "Nesting" [ nested, inline ]
  where
    nested =
        testCase "./tasty/data/nesting0.dhall"
             (Aeson.Object
                 [ ("foo", Aeson.Number 2)
                 , ("name", Aeson.String "Left")
                 ]
             )

    inline =
        testCase "./tasty/data/nesting1.dhall"
            (Aeson.Object
                [ ("name", Aeson.String "Left")
                , ("value", Aeson.Object [ ("foo", Aeson.Number 2 )] )
                ]
            )

    testCase file expectedValue =
        Test.Tasty.HUnit.testCase "Nesting alternative name" $ do
            code <- Data.Text.IO.readFile file

            parsedExpression <- case Dhall.Parser.exprFromText file code of
                Left  exception        -> Exception.throwIO exception
                Right parsedExpression -> return parsedExpression

            resolvedExpression <- Dhall.Import.load parsedExpression

            let mapKey     = "mapKey"
            let mapValue   = "mapValue"
            let conversion = Conversion {..}

            let convertedExpression =
                    Dhall.JSON.convertToHomogeneousMaps conversion resolvedExpression

            actualValue <- case Dhall.JSON.dhallToJSON convertedExpression of
                Left  exception   -> Exception.throwIO exception
                Right actualValue -> return actualValue

            let message = "The alternative name was not nested correctly"

            Test.Tasty.HUnit.assertEqual message expectedValue actualValue

unionKeys :: TestTree
unionKeys = Test.Tasty.HUnit.testCase "Empty alternative" $ do
    let union = Core.Union [ ("A", Nothing), ("B", Nothing) ]
    let schema =
            Core.App
                Core.List
                (Core.Record
                    [ ("mapKey"  , union       )
                    , ("mapValue", Core.Natural)
                    ]
                )

    let expectedValue =
            Aeson.Object [ ("A", Aeson.Number 1), ("B", Aeson.Number 2) ]

    let expectedDhall =
            Core.ListLit
                Nothing
                [   Core.RecordLit
                    [ ("mapKey"  , Core.Field union "A")
                    , ("mapValue", Core.NaturalLit 1   )
                    ]
                ,   Core.RecordLit
                    [ ("mapKey"  , Core.Field union "B")
                    , ("mapValue", Core.NaturalLit 2   )
                    ]
                ]

    actualDhall <- Core.throws (JSONToDhall.dhallFromJSON JSONToDhall.defaultConversion schema expectedValue)

    let message₀ = "Union keys were not decoded from JSON correctly"

    Test.Tasty.HUnit.assertEqual message₀ expectedDhall actualDhall

    let mapKey = "mapKey"

    let mapValue = "mapValue"

    let conversion = Dhall.JSON.Conversion {..}

    let convertedExpression =
            Dhall.JSON.convertToHomogeneousMaps conversion expectedDhall

    let message₁ = "Union keys were not encoded as JSON correctly"

    case Dhall.JSON.dhallToJSON convertedExpression of
        Left exception -> Exception.throwIO exception
        Right actualValue ->
            Test.Tasty.HUnit.assertEqual message₁ expectedValue actualValue
