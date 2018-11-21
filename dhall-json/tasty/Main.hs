{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Dhall.JSON (Conversion(..))
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Parser
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-json"
        [ issue48
        ]

issue48 :: TestTree
issue48 = Test.Tasty.HUnit.testCase "Issue #48" assertion
  where
    assertion = do
        let file = "./tasty/data/issue48.dhall"

        code <- Data.Text.IO.readFile file

        parsedExpression <- case Dhall.Parser.exprFromText file code of
            Left  exception        -> Control.Exception.throwIO exception
            Right parsedExpression -> return parsedExpression

        resolvedExpression <- Dhall.Import.load parsedExpression

        let mapKey     = "mapKey"
        let mapValue   = "mapValue"
        let conversion = Conversion {..}

        let convertedExpression =
                Dhall.JSON.convertToHomogeneousMaps conversion resolvedExpression

        actualValue <- case Dhall.JSON.dhallToJSON convertedExpression of
            Left  exception   -> Control.Exception.throwIO exception
            Right actualValue -> return actualValue

        bytes <- Data.ByteString.Lazy.readFile "./tasty/data/issue48.json"

        expectedValue <- case Data.Aeson.eitherDecode bytes of
            Left  string        -> fail string
            Right expectedValue -> return expectedValue

        let message =
                "Conversion to homogeneous maps did not generate the expected JSON output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue
