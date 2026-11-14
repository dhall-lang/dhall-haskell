{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Bounded (tests) where

import Control.Exception (evaluate)
import Data.Foldable (for_)
import Data.Void (Void)
import Dhall.Bounded (Bound (..))
import GHC.Clock (getMonotonicTime)
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
        [ limitsAreIndependentFlags
        , truncatedRenderingEndsWithEllipsis
        , allocationLimitStopsNormalization
        , outputBudgetStopsQuoting
        , timeLimitInterruptsPureWork
        ]

limitsAreIndependentFlags :: TestTree
limitsAreIndependentFlags = testCase "output, allocation, and time are separate flags" $ do
    case execParserPure defaultPrefs parserInfoOptions [] of
        Success opts -> do
            maxOutputSize opts @?= Nothing
            maxAllocation opts @?= Nothing
            maxEvaluationTime opts @?= Nothing
        other -> assertFailure ("expected success, got " <> showResult other)

    case execParserPure defaultPrefs parserInfoOptions ["--max-output-size=1100"] of
        Success opts -> do
            maxOutputSize opts @?= Just 1100
            maxAllocation opts @?= Nothing
            maxEvaluationTime opts @?= Nothing
        other -> assertFailure ("expected success, got " <> showResult other)

    case execParserPure defaultPrefs parserInfoOptions
            ["--max-output-size=1100", "--max-allocation=1048576", "--max-evaluation-time=30"] of
        Success opts -> do
            maxOutputSize opts @?= Just 1100
            maxAllocation opts @?= Just 1048576
            maxEvaluationTime opts @?= Just 30
        other -> assertFailure ("expected success, got " <> showResult other)

    for_ ["--max-output-size", "--max-allocation", "--max-evaluation-time"] $ \flag ->
        case execParserPure defaultPrefs parserInfoOptions [flag] of
            Failure _ -> return ()
            other -> assertFailure (flag <> " without a value should fail, got " <> showResult other)

    case execParserPure defaultPrefs parserInfoOptions ["--help"] of
        Failure failure -> do
            let (msg, _) = renderFailure failure "dhall"
            assertBool "usage should name the output argument" ("--max-output-size BYTES" `isInfix` msg)
            assertBool "usage should name the allocation argument" ("--max-allocation BYTES" `isInfix` msg)
            assertBool "usage should name the time argument" ("--max-evaluation-time SECONDS" `isInfix` msg)
            assertBool "usage should not offer a bare output flag" (not ("[--max-output-size BYTES | --max-output-size]" `isInfix` msg))
        other ->
            assertFailure ("expected help failure, got " <> showResult other)
  where
    isInfix needle haystack = Text.isInfixOf (Text.pack needle) (Text.pack haystack)

    showResult Success{} = "Success"
    showResult Failure{} = "Failure"
    showResult CompletionInvoked{} = "CompletionInvoked"

allocationLimitStopsNormalization :: TestTree
allocationLimitStopsNormalization = testCase "allocation cap applies with or without an output budget" $ do
    expr <- case Parser.exprFromText mempty source of
        Left err -> assertFailure (show err)
        Right parsed -> return (Core.denote parsed :: Core.Expr Void Core.Import)
    over <- Bounded.normalizeLimited (Just 1) Nothing Nothing expr
        :: IO (Either Bounded.WorkLimit (Core.Expr Void Core.Import, Bool))
    case over of
        Left Bounded.AllocationExceeded -> return ()
        Left other -> assertFailure (show other)
        Right _ -> assertFailure "normalization finished within the allocation cap"
    combined <- Bounded.normalizeLimited (Just 1) Nothing (Just 1000000) expr
        :: IO (Either Bounded.WorkLimit (Core.Expr Void Core.Import, Bool))
    case combined of
        Left Bounded.AllocationExceeded -> return ()
        Left other -> assertFailure (show other)
        Right _ -> assertFailure "output budget disabled the allocation cap"
  where
    source =
        "Natural/fold 4000 (List Natural) (\\(x : Natural) -> \\(xs : List Natural) -> [x] # xs) ([] : List Natural)"

timeLimitInterruptsPureWork :: TestTree
timeLimitInterruptsPureWork = testCase "time limit interrupts pure work" $ do
    start <- getMonotonicTime
    result <- Bounded.runLimited Nothing (Just 100000) (evaluate (burn 80000000))
    elapsed <- fmap (subtract start) getMonotonicTime
    case result of
        Left Bounded.TimeExceeded ->
            assertBool
                ("time limit returned too late: " <> show elapsed)
                (elapsed < 2)
        Left other ->
            assertFailure (show other <> " after " <> show elapsed)
        Right _ ->
            assertFailure ("pure work finished inside the time limit after " <> show elapsed)
  where
    -- 'show' allocates, so the loop hits the heap checks where the watcher
    -- can interrupt it.  A tight unboxed loop would not.
    burn :: Int -> Int
    burn n = go n 0
      where
        go 0 acc = acc
        go i acc = go (i - 1) (acc + length (show i))

outputBudgetStopsQuoting :: TestTree
outputBudgetStopsQuoting = testCase "output bytes stop quoting a large record early" $ do
    let fields =
            Text.intercalate ", "
                [ "f" <> Text.pack (show i) <> " = " <> Text.pack (show i) | i <- [1 .. 80 :: Int] ]
        source = "{ " <> fields <> " }"
    expr <- case Parser.exprFromText mempty source of
        Left err -> assertFailure (show err)
        Right parsed -> return (Core.denote parsed :: Core.Expr Void Core.Import)
    quoted <- Bounded.normalizeLimited Nothing Nothing (Just 80) expr
        :: IO (Either Bounded.WorkLimit (Core.Expr Parser.Src Core.Import, Bool))
    case quoted of
        Left reason -> assertFailure (show reason)
        Right (_, False) -> assertFailure "expected quoting to stop"
        Right (partial, True) -> do
            let doc = Pretty.prettyCharacterSet Pretty.ASCII partial
            case Bounded.prettyBounded (maxBound :: Int) doc of
                Complete text ->
                    assertBool
                        ("quoted form is still too large: " <> show (Text.length text))
                        (Text.length text < 400)
                Truncated text ->
                    assertFailure ("prettyBounded unexpectedly truncated " <> Text.unpack text)

truncatedRenderingEndsWithEllipsis :: TestTree
truncatedRenderingEndsWithEllipsis = testCase "truncation ends with an ellipsis" $ do
    expr <- case Parser.exprFromText mempty source of
        Left err -> assertFailure (show err)
        Right parsed -> return (Core.denote parsed)
    let doc = Pretty.prettyCharacterSet Pretty.ASCII expr
        full = case Bounded.prettyBounded (maxBound :: Int) doc of
            Complete text -> text
            Truncated text -> text
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
