{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.Lazy.IO
import qualified Dhall.Parser
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Util

parserTests :: TestTree
parserTests =
    Test.Tasty.testGroup "parser tests"
        [ Test.Tasty.testGroup "whitespace"
            [ shouldPass
                "prefix/suffix"
                "./tests/parser/whitespace.dhall"
            , shouldPass
                "block comment"
                "./tests/parser/blockComment.dhall"
            , shouldPass
                "nested block comment"
                "./tests/parser/nestedBlockComment.dhall"
            , shouldPass
                "line comment"
                "./tests/parser/lineComment.dhall"
            , shouldPass
                "Unicode comment"
                "./tests/parser/unicodeComment.dhall"
            , shouldPass
                "whitespace buffet"
                "./tests/parser/whitespaceBuffet.dhall"
            , shouldPass
                "label"
                "./tests/parser/label.dhall"
            , shouldPass
                "quoted label"
                "./tests/parser/quotedLabel.dhall"
            , shouldPass
                "double quoted string"
                "./tests/parser/doubleQuotedString.dhall"
            , shouldPass
                "Unicode double quoted string"
                "./tests/parser/unicodeDoubleQuotedString.dhall"
            , shouldPass
                "escaped double quoted string"
                "./tests/parser/escapedDoubleQuotedString.dhall"
            , shouldPass
                "interpolated double quoted string"
                "./tests/parser/interpolatedDoubleQuotedString.dhall"
            , shouldPass
                "single quoted string"
                "./tests/parser/singleQuotedString.dhall"
            , shouldPass
                "escaped single quoted string"
                "./tests/parser/escapedSingleQuotedString.dhall"
            , shouldPass
                "interpolated single quoted string"
                "./tests/parser/interpolatedSingleQuotedString.dhall"
            , shouldPass
                "double"
                "./tests/parser/double.dhall"
            , shouldPass
                "natural"
                "./tests/parser/natural.dhall"
            , shouldPass
                "identifier"
                "./tests/parser/identifier.dhall"
            , shouldParse
                "paths"
                "./tests/parser/paths.dhall"
            , shouldParse
                "path termination"
                "./tests/parser/pathTermination.dhall"
            , shouldParse
                "urls"
                "./tests/parser/urls.dhall"
            , shouldParse
                "environmentVariables"
                "./tests/parser/environmentVariables.dhall"
            , shouldPass
                "lambda"
                "./tests/parser/lambda.dhall"
            , shouldPass
                "if then else"
                "./tests/parser/ifThenElse.dhall"
            , shouldPass
                "let"
                "./tests/parser/let.dhall"
            , shouldPass
                "forall"
                "./tests/parser/forall.dhall"
            , shouldPass
                "function type"
                "./tests/parser/functionType.dhall"
            , shouldPass
                "operators"
                "./tests/parser/operators.dhall"
            , shouldPass
                "annotations"
                "./tests/parser/annotations.dhall"
            , shouldPass
                "merge"
                "./tests/parser/merge.dhall"
            , shouldPass
                "constructors"
                "./tests/parser/constructors.dhall"
            , shouldPass
                "fields"
                "./tests/parser/fields.dhall"
            , shouldPass
                "record"
                "./tests/parser/record.dhall"
            , shouldPass
                "union"
                "./tests/parser/union.dhall"
            , shouldPass
                "list"
                "./tests/parser/list.dhall"
            , shouldPass
                "builtins"
                "./tests/parser/builtins.dhall"
            , shouldPass
                "large expression"
                "./tests/parser/largeExpression.dhall"
            , shouldPass
                "names that begin with reserved identifiers"
                "./tests/parser/reservedPrefix.dhall"
            ]
        ]

shouldPass :: Text -> Text -> TestTree
shouldPass name code = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    _ <- Util.code code
    return () )

shouldParse :: Text -> FilePath -> TestTree
shouldParse name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.Lazy.IO.readFile path
    case Dhall.Parser.exprFromText mempty text of
        Left err -> Control.Exception.throwIO err
        Right _  -> return () )
