{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Parser where

import           Data.Text            (Text)
import           Test.Tasty           (TestTree)

import qualified Codec.Serialise
import qualified Control.Exception
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.IO
import qualified Dhall.Binary
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
            "builtin name as label"
            "./dhall-lang/tests/parser/success/builtinNameAsField"
        , shouldParse
            "quoted label"
            "./dhall-lang/tests/parser/success/quotedLabel"
        , shouldParse
            "double quoted string"
            "./dhall-lang/tests/parser/success/text/doubleQuotedString"
        , shouldParse
            "Unicode double quoted string"
            "./dhall-lang/tests/parser/success/text/unicodeDoubleQuotedString"
        , shouldParse
            "escaped double quoted string"
            "./dhall-lang/tests/parser/success/text/escapedDoubleQuotedString"
        , shouldParse
            "interpolated double quoted string"
            "./dhall-lang/tests/parser/success/text/interpolatedDoubleQuotedString"
        , shouldParse
            "single quoted string"
            "./dhall-lang/tests/parser/success/text/singleQuotedString"
        , shouldParse
            "escaped single quoted string"
            "./dhall-lang/tests/parser/success/text/escapedSingleQuotedString"
        , shouldParse
            "interpolated single quoted string"
            "./dhall-lang/tests/parser/success/text/interpolatedSingleQuotedString"
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
            "./dhall-lang/tests/parser/success/import/paths"
        , shouldParse
            "path termination"
            "./dhall-lang/tests/parser/success/import/pathTermination"
        , shouldParse
            "urls"
            "./dhall-lang/tests/parser/success/import/urls"
        , shouldParse
            "environmentVariables"
            "./dhall-lang/tests/parser/success/import/environmentVariables"
        , shouldParse
            "hash"
            "./dhall-lang/tests/parser/success/import/hash"
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
            "./dhall-lang/tests/parser/success/import/importAlt"
        , shouldParse
            "large expression"
            "./dhall-lang/tests/parser/success/largeExpression"
        , shouldParse
            "names that begin with reserved identifiers"
            "./dhall-lang/tests/parser/success/reservedPrefix"
        , shouldParse
            "interpolated expressions with leading whitespace"
            "./dhall-lang/tests/parser/success/text/template"
        , shouldParse
            "collections with type annotations containing imports"
            "./dhall-lang/tests/parser/success/collectionImportType"
        , shouldParse
            "a parenthesized custom header import"
            "./dhall-lang/tests/parser/success/import/parenthesizeUsing"
        , shouldNotParse
            "accessing a field of an import without parentheses"
            "./dhall-lang/tests/parser/failure/importAccess.dhall"
        , shouldParse
            "Sort"
            "./dhall-lang/tests/parser/success/sort"
        , shouldParse
            "quoted path components"
            "./dhall-lang/tests/parser/success/import/quotedPaths"
        , shouldNotParse
            "positive double out of bounds"
            "./dhall-lang/tests/parser/failure/doubleBoundsPos.dhall"
        , shouldNotParse
            "negative double out of bounds"
            "./dhall-lang/tests/parser/failure/doubleBoundsNeg.dhall"
        , shouldParse
            "as Text"
            "./dhall-lang/tests/parser/success/import/asText"
        , shouldNotParse
            "a multi-line literal without an initial newline"
            "./dhall-lang/tests/parser/failure/mandatoryNewline.dhall"
        , shouldParse
            "a Unicode path component"
            "./dhall-lang/tests/parser/success/import/unicodePaths"
        ]

multiline :: TestTree
multiline =
    Test.Tasty.testGroup "Multi-line literals"
        [ shouldParse
            "multi-line escape sequences"
            "./dhall-lang/tests/parser/success/text/escape"
        , shouldParse
            "a multi-line literal with a hanging indent"
            "./dhall-lang/tests/parser/success/text/hangingIndent"
        , shouldParse
            "a multi-line literal with an interior indent"
            "./dhall-lang/tests/parser/success/text/interiorIndent"
        , shouldParse
            "a multi-line literal with an interpolated expression"
            "./dhall-lang/tests/parser/success/text/interpolation"
        , shouldParse
            "comments within a multi-line literal"
            "./dhall-lang/tests/parser/success/text/preserveComment"
        , shouldParse
            "a multi-line literal with one line"
            "./dhall-lang/tests/parser/success/text/singleLine"
        , shouldParse
            "a multi-line literal with two lines"
            "./dhall-lang/tests/parser/success/text/twoLines"
        ]

shouldParse :: Text -> FilePath -> TestTree
shouldParse name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
    text    <- Data.Text.IO.readFile (path <> "A.dhall")
    encoded <- Data.ByteString.Lazy.readFile (path <> "B.dhallb")

    expression <- case Dhall.Parser.exprFromText mempty text of
      Left e -> Control.Exception.throwIO e
      Right a -> pure a

    let term  = Dhall.Binary.encode expression
        bytes = Codec.Serialise.serialise term

    let message = "The expected CBOR representation doesn't match the actual one"
    Test.Tasty.HUnit.assertEqual message encoded bytes

shouldNotParse :: Text -> FilePath -> TestTree
shouldNotParse name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    text <- Data.Text.IO.readFile path
    case Dhall.Parser.exprFromText mempty text of
        Left  _ -> return ()
        Right _ -> fail "Unexpected successful parser" )
