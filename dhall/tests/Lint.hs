{-# LANGUAGE OverloadedStrings #-}

module Lint where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Lint
import qualified Dhall.Parser
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

lintTests :: TestTree
lintTests =
    Test.Tasty.testGroup "format tests"
        [ should
            "correctly handle multi-let expressions"
            "success/multilet"
        ]

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let inputFile =
                Data.Text.unpack ("./tests/lint/" <> basename <> "A.dhall")
        let outputFile =
                Data.Text.unpack ("./tests/lint/" <> basename <> "B.dhall")

        inputText <- Data.Text.IO.readFile inputFile

        parsedInput <- case Dhall.Parser.exprFromText mempty inputText of
            Left  exception  -> Control.Exception.throwIO exception
            Right expression -> return expression

        let lintedInput = Dhall.Lint.lint parsedInput

        actualExpression <- Dhall.Import.load lintedInput

        outputText <- Data.Text.IO.readFile outputFile

        parsedOutput <- case Dhall.Parser.exprFromText mempty outputText of
            Left  exception  -> Control.Exception.throwIO exception
            Right expression -> return expression

        resolvedOutput <- Dhall.Import.load parsedOutput

        let expectedExpression = Dhall.Core.denote resolvedOutput

        let message =
                "The linted expression did not match the expected output"

        Test.Tasty.HUnit.assertEqual message expectedExpression actualExpression
