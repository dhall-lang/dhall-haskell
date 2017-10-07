{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Data.Text
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
            ]
        ]

shouldPass :: Text -> Text -> TestTree
shouldPass name code = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    _ <- Util.code code
    return () )
