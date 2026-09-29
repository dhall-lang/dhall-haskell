{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Bounded (tests) where

import Data.Foldable (for_)
import Dhall.Bounded (Bound (..))
import Dhall.Main (Options (..), parserInfoOptions)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, renderFailure)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified Data.Text as Text
import qualified Dhall.Bounded as Bounded
import qualified Dhall.Core as Core
import qualified Dhall.Parser as Parser
import qualified Dhall.Pretty as Pretty
import qualified Test.Tasty

tests :: TestTree
tests =
    Test.Tasty.testGroup "bounded output"
        [ maxOutputSizeRequiresAnArgument
        , truncatedRenderingEndsWithEllipsis
        ]

maxOutputSizeRequiresAnArgument :: TestTree
maxOutputSizeRequiresAnArgument = testCase "requires BYTES and has no default" $ do
    case execParserPure defaultPrefs parserInfoOptions [] of
        Success opts -> maxOutputSize opts @?= Nothing
        other -> assertFailure ("expected success, got " <> showResult other)

    case execParserPure defaultPrefs parserInfoOptions ["--max-output-size=1100"] of
        Success opts -> maxOutputSize opts @?= Just 1100
        other -> assertFailure ("expected success, got " <> showResult other)

    case execParserPure defaultPrefs parserInfoOptions ["--max-output-size"] of
        Failure _ -> return ()
        other -> assertFailure ("bare flag should fail, got " <> showResult other)

    case execParserPure defaultPrefs parserInfoOptions ["--help"] of
        Failure failure -> do
            let (msg, _) = renderFailure failure "dhall"
            assertBool "usage should name the argument" ("--max-output-size BYTES" `isInfix` msg)
            assertBool "usage should not offer a bare flag" (not ("[--max-output-size BYTES | --max-output-size]" `isInfix` msg))
            assertBool "help should not mention a 128KiB default" (not ("128KiB" `isInfix` msg))
        other ->
            assertFailure ("expected help failure, got " <> showResult other)
  where
    isInfix needle haystack = Text.isInfixOf (Text.pack needle) (Text.pack haystack)

    showResult Success{} = "Success"
    showResult Failure{} = "Failure"
    showResult CompletionInvoked{} = "CompletionInvoked"

truncatedRenderingEndsWithEllipsis :: TestTree
truncatedRenderingEndsWithEllipsis = testCase "truncation ends with an ellipsis" $ do
    expr <- case Parser.exprFromText mempty source of
        Left err -> assertFailure (show err)
        Right parsed -> return (Core.denote parsed)
    let doc = Pretty.prettyCharacterSet Pretty.ASCII expr
        full = case Bounded.prettyBounded (maxBound :: Int) doc of
            Complete text -> text
            Truncated text -> text
            LimitExceeded -> "limit"
    assertBool "fixture is long enough to truncate" (Text.length full > 40)
    for_ [1, 5, 8, 17, 40 :: Int] $ \budget ->
        case Bounded.prettyBounded budget doc of
            Truncated text -> do
                assertBool (show budget <> " should end with ellipsis: " <> show text) (Text.isSuffixOf "…" text)
                let body = Text.dropEnd 1 text
                assertBool (show budget <> " should be a prefix: " <> show text) (Text.isPrefixOf body full)
                assertBool (show budget <> " should not continue after the ellipsis") (not (Text.isInfixOf "…" body))
            other ->
                assertFailure (show budget <> " expected Truncated, got " <> show other)
  where
    source =
        "{ parameters.type = \"pd-ssd\"\n\
        \, ClusterRole.backend = { name = \"backend\", namespace = Some \"ns\" }\n\
        \, StorageClass.sourcegraph = { provisioner = \"kubernetes.io/gce-pd\" }\n\
        \}"
