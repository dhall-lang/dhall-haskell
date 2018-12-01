{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Parser
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

parserTests :: TestTree
parserTests =
    Test.Tasty.testGroup "parser tests"
        [ Test.Tasty.testGroup "whitespace"
            [ shouldParse
                "prefix/suffix"
                "./tests/parser/success/whitespace"
            , shouldParse
                "block comment"
                "./tests/parser/success/blockComment"
            , shouldParse
                "nested block comment"
                "./tests/parser/success/nestedBlockComment"
            , shouldParse
                "line comment"
                "./tests/parser/success/lineComment"
            , shouldParse
                "Unicode comment"
                "./tests/parser/success/unicodeComment"
            , shouldParse
                "whitespace buffet"
                "./tests/parser/success/whitespaceBuffet"
            ]
        , shouldParse
            "label"
            "./tests/parser/success/label"
        , shouldParse
            "quoted label"
            "./tests/parser/success/quotedLabel"
        , shouldParse
            "double quoted string"
            "./tests/parser/success/doubleQuotedString"
        , shouldParse
            "Unicode double quoted string"
            "./tests/parser/success/unicodeDoubleQuotedString"
        , shouldParse
            "escaped double quoted string"
            "./tests/parser/success/escapedDoubleQuotedString"
        , shouldParse
            "interpolated double quoted string"
            "./tests/parser/success/interpolatedDoubleQuotedString"
        , shouldParse
            "single quoted string"
            "./tests/parser/success/singleQuotedString"
        , shouldParse
            "escaped single quoted string"
            "./tests/parser/success/escapedSingleQuotedString"
        , shouldParse
            "interpolated single quoted string"
            "./tests/parser/success/interpolatedSingleQuotedString"
        , shouldParse
            "double"
            "./tests/parser/success/double"
        , shouldParse
            "natural"
            "./tests/parser/success/natural"
        , shouldParse
            "identifier"
            "./tests/parser/success/identifier"
        , shouldParse
            "paths"
            "./tests/parser/success/paths"
        , shouldParse
            "path termination"
            "./tests/parser/success/pathTermination"
        , shouldParse
            "urls"
            "./tests/parser/success/urls"
        , shouldParse
            "environmentVariables"
            "./tests/parser/success/environmentVariables"
        , shouldParse
            "lambda"
            "./tests/parser/success/lambda"
        , shouldParse
            "if then else"
            "./tests/parser/success/ifThenElse"
        , shouldParse
            "let"
            "./tests/parser/success/let"
        , shouldParse
            "forall"
            "./tests/parser/success/forall"
        , shouldParse
            "function type"
            "./tests/parser/success/functionType"
        , shouldParse
            "operators"
            "./tests/parser/success/operators"
        , shouldParse
            "annotations"
            "./tests/parser/success/annotations"
        , shouldParse
            "merge"
            "./tests/parser/success/merge"
        , shouldParse
            "constructors"
            "./tests/parser/success/constructors"
        , shouldParse
            "fields"
            "./tests/parser/success/fields"
        , shouldParse
            "record"
            "./tests/parser/success/record"
        , shouldParse
            "union"
            "./tests/parser/success/union"
        , shouldParse
            "list"
            "./tests/parser/success/list"
        , shouldParse
            "builtins"
            "./tests/parser/success/builtins"
        , shouldParse
            "import alternatives"
            "./tests/parser/success/importAlt"
        , shouldParse
            "large expression"
            "./tests/parser/success/largeExpression"
        , shouldParse
            "names that begin with reserved identifiers"
            "./tests/parser/success/reservedPrefix"
        , shouldParse
            "interpolated expressions with leading whitespace"
            "./tests/parser/success/template"
        , shouldParse
            "collections with type annotations containing imports"
            "./tests/parser/success/collectionImportType"
        , shouldParse
            "a parenthesized custom header import"
            "./tests/parser/success/parenthesizeUsing"
        , shouldNotParse
            "accessing a field of an import without parentheses"
            "./tests/parser/failure/importAccess.dhall"
        , shouldParse
            "Sort"
            "./tests/parser/success/sort"
        , shouldParse
            "quoted path components"
            "./tests/parser/success/quotedPaths"
        , shouldNotParse
            "positive double out of bounds"
            "./tests/parser/failure/doubleBoundsPos.dhall"
        , shouldNotParse
            "negative double out of bounds"
            "./tests/parser/failure/doubleBoundsNeg.dhall"
        , shouldParse
            "as Text"
            "./tests/parser/success/asText"
        , shouldNotParse
            "a multi-line literal without an initial newline"
            "./tests/parser/failure/mandatoryNewline.dhall"
        ]

shouldParse :: Text -> FilePath -> TestTree
shouldParse name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.IO.readFile (path <> "A.dhall")
    case Dhall.Parser.exprFromText mempty text of
        Left err -> Control.Exception.throwIO err
        Right _  -> return () )

shouldNotParse :: Text -> FilePath -> TestTree
shouldNotParse name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.IO.readFile path
    case Dhall.Parser.exprFromText mempty text of
        Left  _ -> return ()
        Right _ -> fail "Unexpected successful parser" )
