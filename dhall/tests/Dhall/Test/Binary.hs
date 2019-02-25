{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Binary where

import           Data.Text         (Text)
import           Test.Tasty        (TestTree)

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
    Test.Tasty.testGroup "binary format tests"
        [ shouldMatch
            "binary encoding of Double values"
            "./dhall-lang/tests/binary/success/double"
        ]

shouldMatch :: Text -> FilePath -> TestTree
shouldMatch name path = Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
    text    <- Data.Text.IO.readFile (path <> "A.dhall")
    encoded <- Data.ByteString.Lazy.readFile (path <> "B.dhallb")

    expression <- case Dhall.Parser.exprFromText mempty text of
      Left e -> Control.Exception.throwIO e
      Right a -> pure a

    let term  = Dhall.Binary.encode expression
        bytes = Codec.Serialise.serialise term

    case encoded == bytes of
        True -> return ()
        _    -> error "Binary representation doesn't match"
