{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Parser where

import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Parser
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

tests :: TestTree
tests =
    Test.Tasty.testGroup "parser tests"
        [ Test.Tasty.testGroup "whitespace"
            [ shouldParse
                "prefix/suffix"
                "./dhall-lang/tests/parser/success/whitespace"
            , shouldParse
                "block comment"
                "./dhall-lang/tests/parser/success/blockComment"
            , shouldParse
                "nested block comment"
                "./dhall-lang/tests/parser/success/nestedBlockComment"
            , shouldParse
                "line comment"
                "./dhall-lang/tests/parser/success/lineComment"
            , shouldParse
                "Unicode comment"
                "./dhall-lang/tests/parser/success/unicodeComment"
            , shouldParse
                "whitespace buffet"
                "./dhall-lang/tests/parser/success/whitespaceBuffet"
            ]
        , shouldParse
            "label"
            "./dhall-lang/tests/parser/success/label"
        , shouldParse
            "quoted label"
            "./dhall-lang/tests/parser/success/quotedLabel"
        , shouldParse
            "double quoted string"
            "./dhall-lang/tests/parser/success/doubleQuotedString"
        , shouldParse
            "Unicode double quoted string"
            "./dhall-lang/tests/parser/success/unicodeDoubleQuotedString"
        , shouldParse
            "escaped double quoted string"
            "./dhall-lang/tests/parser/success/escapedDoubleQuotedString"
        , shouldParse
            "interpolated double quoted string"
            "./dhall-lang/tests/parser/success/interpolatedDoubleQuotedString"
        , shouldParse
            "single quoted string"
            "./dhall-lang/tests/parser/success/singleQuotedString"
        , shouldParse
            "escaped single quoted string"
            "./dhall-lang/tests/parser/success/escapedSingleQuotedString"
        , shouldParse
            "interpolated single quoted string"
            "./dhall-lang/tests/parser/success/interpolatedSingleQuotedString"
        , shouldParse
            "double"
            "./dhall-lang/tests/parser/success/double"
        , shouldParse
            "natural"
            "./dhall-lang/tests/parser/success/natural"
        , shouldParse
            "identifier"
            "./dhall-lang/tests/parser/success/identifier"
        , shouldParse
            "paths"
            "./dhall-lang/tests/parser/success/paths"
        , shouldParse
            "path termination"
            "./dhall-lang/tests/parser/success/pathTermination"
        , shouldParse
            "urls"
            "./dhall-lang/tests/parser/success/urls"
        , shouldParse
            "environmentVariables"
            "./dhall-lang/tests/parser/success/environmentVariables"
        , shouldParse
            "lambda"
            "./dhall-lang/tests/parser/success/lambda"
        , shouldParse
            "if then else"
            "./dhall-lang/tests/parser/success/ifThenElse"
        , shouldParse
            "let"
            "./dhall-lang/tests/parser/success/let"
        , shouldParse
            "forall"
            "./dhall-lang/tests/parser/success/forall"
        , shouldParse
            "function type"
            "./dhall-lang/tests/parser/success/functionType"
        , shouldParse
            "operators"
            "./dhall-lang/tests/parser/success/operators"
        , shouldParse
            "annotations"
            "./dhall-lang/tests/parser/success/annotations"
        , shouldParse
            "merge"
            "./dhall-lang/tests/parser/success/merge"
        , shouldParse
            "constructors"
            "./dhall-lang/tests/parser/success/constructors"
        , shouldParse
            "fields"
            "./dhall-lang/tests/parser/success/fields"
        , shouldParse
            "record"
            "./dhall-lang/tests/parser/success/record"
        , shouldParse
            "union"
            "./dhall-lang/tests/parser/success/union"
        , shouldParse
            "list"
            "./dhall-lang/tests/parser/success/list"
        , shouldParse
            "builtins"
            "./dhall-lang/tests/parser/success/builtins"
        , shouldParse
            "import alternatives"
            "./dhall-lang/tests/parser/success/importAlt"
        , shouldParse
            "large expression"
            "./dhall-lang/tests/parser/success/largeExpression"
        , shouldParse
            "names that begin with reserved identifiers"
            "./dhall-lang/tests/parser/success/reservedPrefix"
        , shouldParse
            "interpolated expressions with leading whitespace"
            "./dhall-lang/tests/parser/success/template"
        , shouldParse
            "collections with type annotations containing imports"
            "./dhall-lang/tests/parser/success/collectionImportType"
        , shouldParse
            "a parenthesized custom header import"
            "./dhall-lang/tests/parser/success/parenthesizeUsing"
        , shouldNotParse
            "accessing a field of an import without parentheses"
            "./dhall-lang/tests/parser/failure/importAccess.dhall"
        , shouldParse
            "Sort"
            "./dhall-lang/tests/parser/success/sort"
        , shouldParse
            "quoted path components"
            "./dhall-lang/tests/parser/success/quotedPaths"
        , shouldNotParse
            "positive double out of bounds"
            "./dhall-lang/tests/parser/failure/doubleBoundsPos.dhall"
        , shouldNotParse
            "negative double out of bounds"
            "./dhall-lang/tests/parser/failure/doubleBoundsNeg.dhall"
        , shouldParse
            "as Text"
            "./dhall-lang/tests/parser/success/asText"
        , shouldNotParse
            "a multi-line literal without an initial newline"
            "./dhall-lang/tests/parser/failure/mandatoryNewline.dhall"
        , shouldParse
            "a Unicode path component"
            "./dhall-lang/tests/parser/success/unicodePaths"
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
