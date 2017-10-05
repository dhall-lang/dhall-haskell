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
            [ shouldPass "prefix/suffix" "./tests/parser/whitespace.dhall"
            , shouldPass "block comment" "./tests/parser/blockComment.dhall"
            ]
        ]

shouldPass :: Text -> Text -> TestTree
shouldPass name code = Test.Tasty.HUnit.testCase (Data.Text.unpack name) (do
    _ <- Util.code code
    return () )
