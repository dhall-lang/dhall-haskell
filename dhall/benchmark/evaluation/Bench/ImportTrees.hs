{-# LANGUAGE OverloadedStrings #-}
-- | Import-tree evaluation fixtures: normalize/*, large1–large6, k8s, prelude.
module Bench.ImportTrees
    ( benchmarks
    ) where

import Control.Exception (throw)
import Control.Monad (when)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Text (Text)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import Test.Tasty.Bench

import Bench.Common

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Import as Import
import qualified Dhall.Parser as Parser
import qualified Lens.Micro
import qualified System.Directory as Directory

large1Directory :: FilePath
large1Directory = "benchmark/evaluation/large1"

large1MainPath :: FilePath
large1MainPath = large1Directory </> "main.dhall"

large1Settings :: Dhall.InputSettings
large1Settings =
    Lens.Micro.set Dhall.sourceName large1MainPath
        (Lens.Micro.set Dhall.rootDirectory large1Directory Dhall.defaultInputSettings)

large2Directory :: FilePath
large2Directory = "benchmark/evaluation/large2"

large2MainPath :: FilePath
large2MainPath = large2Directory </> "main.dhall"

large3Directory :: FilePath
large3Directory = "benchmark/evaluation/large3"

large4Directory :: FilePath
large4Directory = "benchmark/evaluation/large4"

large5Directory :: FilePath
large5Directory = "benchmark/evaluation/large5"

large6Directory :: FilePath
large6Directory = "benchmark/evaluation/large6"

preludeImportDirectory :: FilePath
preludeImportDirectory = "benchmark/evaluation/prelude_import"

k8sDirectory :: FilePath
k8sDirectory = "benchmark/evaluation/k8s"

normalizeLabels :: String -> [String]
normalizeLabels name =
    [ "normalize." <> name
    , "normalize." <> name <> ".typecheck"
    , "normalize." <> name <> ".evaluation"
    ]

large1Labels :: [String]
large1Labels =
    [ "large1"
    , "large1.parse"
    , "large1.resolve"
    , "large1.typecheck"
    , "large1.evaluation"
    ]

large2Labels :: [String]
large2Labels =
    [ "large2"
    , "large2.normalize"
    , "large2.cbor.encode"
    , "large2.cbor.decode"
    ]

large3Labels :: [String]
large3Labels = phaseLabels "large3"

large3GetConfigLabels :: [String]
large3GetConfigLabels = phaseLabels "large3.get_config"

large4Labels :: [String]
large4Labels = phaseLabels "large4"

large5CodeLabels :: [String]
large5CodeLabels = phaseLabels "large5"

preludeImportCodeLabels :: [String]
preludeImportCodeLabels = coldResolveLabels "prelude_import"

k8sLabels :: String -> [String]
k8sLabels name =
    [ "k8s." <> name
    , "k8s." <> name <> ".resolve"
    , "k8s." <> name <> ".typecheck"
    , "k8s." <> name <> ".evaluation"
    ]

loadExamples :: Maybe String -> IO [(String, ResolvedExpr)]
loadExamples mPattern = do
    files <- sort <$> Directory.listDirectory normalizeDirectory
    let paths =
            [ normalizeDirectory </> file
            | file <- files
            , ".dhall" `isSuffixOf` file
            , let name = takeBaseName file
            , any (couldMatch mPattern) (normalizeLabels name)
            ]
    if null paths
        then do
            say "Skipping normalize fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading normalize fixtures (" <> show (length paths) <> " file(s))…"
            traverse loadExample paths
  where
    normalizeDirectory = "benchmark/evaluation/normalize"

loadExample :: FilePath -> IO (String, ResolvedExpr)
loadExample path = do
    let name = takeBaseName path
    let prefix = "normalize/" <> name

    text <- timed (prefix <> ": read") (Text.IO.readFile path)

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText path text)

    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)

    resolved <- timed (prefix <> ": resolve (cache on)") $
        resolveWithCache settings parsed

    timed (prefix <> ": typecheck") (ensureWellTyped resolved)
    say $ "  " <> prefix <> ": ready"

    pure (name, resolved)

loadLarge6PhaseVariants :: Maybe String -> IO [PipelineBench]
loadLarge6PhaseVariants mPattern = do
    -- Mode A large6 rows (see large6/README.md matrix).
    let candidates =
            [ ("large6.slow_parse", "pipeline-code-long-parse.dhall")
            , ("large6.slow_walk", "pipeline-code-long-walk.dhall")
            ]
        selected =
            [ entry
            | entry@(label, _) <- candidates
            , any (couldMatch mPattern) (phaseLabels label)
            ]
    if null selected
        then do
            say "Skipping large6 fixtures (do not match pattern)"
            pure []
        else do
            when (any (("slow_parse" `isInfixOf`) . fst) selected) $
                ensureSlowParseDhall large6Directory
            say $ "Loading large6 fixtures (" <> show (length selected) <> " file(s))…"
            traverse (\(label, file) -> loadPipelineBench label large6Directory file) selected

-- | Always rewrite `slow/parse.dhall`. Nested block comments (~1.3M lines)
-- give an artificial ~0.5s parse burden; the file evaluates to `True`.
--
-- Keep the block count / nesting in sync with `slow/generate-parse.py`.
-- Do not reuse a leftover file: a previous run with different nesting or
-- block count would silently change the timed parse cost.
ensureSlowParseDhall :: FilePath -> IO ()
ensureSlowParseDhall directory = do
    let path = directory </> "slow" </> "parse.dhall"
    say $ "Generating parse fixture: " <> path
    Directory.createDirectoryIfMissing True (takeDirectory path)
    Text.IO.writeFile path slowParseDhallContents

slowParseDhallContents :: Text
slowParseDhallContents =
    header <> Text.replicate blockCount block <> "True\n"
  where
    header =
        "-- Artificial parse burden: ~0.5 seconds to parse this file (cold, no cache).\n\
        \-- The cost is in the lexer/parser walking ~1.3M lines of nested block comments;\n\
        \-- the normalized value is just `True`.\n\
        \-- Generated during evaluation-benchmark setup (see Bench.ImportTrees).\n"
    nesting = 8
    blockCount = 81270
    block = Text.replicate nesting "{-\n" <> Text.replicate nesting "-}\n"

loadLarge6ColdResolveVariants :: Maybe String -> IO [ColdResolveBench]
loadLarge6ColdResolveVariants mPattern = do
    -- Mode B large6 Code rows where prep would hide resolve cost.
    let candidates =
            [ ("large6.slow_eval", "pipeline-code-long-eval.dhall")
            , ("large6.slow_typecheck", "pipeline-code-long-typecheck.dhall")
            , ("large6.slow_normalize", "pipeline-code-long-normalize.dhall")
            , ("large6.slow_multi", "pipeline-code-long-multi.dhall")
            ]
        selected =
            [ entry
            | entry@(label, _) <- candidates
            , any (couldMatch mPattern) (coldResolveLabels label)
            ]
    if null selected
        then do
            say "Skipping large6 cold-resolve fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading large6 cold-resolve fixtures (" <> show (length selected) <> " file(s))…"
            traverse (\(label, file) -> loadColdResolveBench label large6Directory file) selected

-- | Load large1 inputs for the existing per-phase benchmarks.
--
-- Import resolution for prep uses 'resolveWithCache'. The timed @resolve@
-- bench re-runs resolution with 'resolveWithoutCache' via 'nfAppIO'.
loadLarge1 :: IO (Text, ParsedExpr, ResolvedExpr)
loadLarge1 = do
    say "Loading large1…"

    text <- timed "large1: read" (Text.IO.readFile large1MainPath)

    parsed <- timed "large1: parse" $
        either throw pure (Parser.exprFromText large1MainPath text)

    resolved <- timed "large1: resolve (cache on)" $
        resolveWithCache large1Settings parsed

    timed "large1: typecheck" (ensureWellTyped resolved)

    say "  large1: ready"

    pure (text, parsed, resolved)

-- | Load large2 for CBOR benchmarking: parse, resolve, typecheck, then
-- pre-encode once to obtain decode input and report CBOR size.
loadLarge2 :: IO (ResolvedExpr, ResolvedExpr, ByteString.ByteString)
loadLarge2 = do
    say "Loading large2…"

    text <- timed "large2: read" (Text.IO.readFile large2MainPath)

    parsed <- timed "large2: parse" $
        either throw pure (Parser.exprFromText large2MainPath text)

    resolved <- timed "large2: resolve (assert no imports)" $
        Import.assertNoImports parsed

    timed "large2: typecheck" (ensureWellTyped resolved)

    normalized <- timed "large2: normalize (for encode/decode fixture)" $
        pure (Core.normalize resolved)

    encoded <- timed "large2: encode CBOR (for decode fixture)" $
        pure (encodeNormalized normalized)

    let cborBytes = ByteString.length encoded
    say $ "  large2: CBOR size: " <> show cborBytes <> " bytes"

    say "  large2: ready"

    pure (resolved, normalized, encoded)

k8sSettings :: FilePath -> Dhall.InputSettings
k8sSettings sourceName =
    Lens.Micro.set Dhall.sourceName sourceName
        (Lens.Micro.set Dhall.rootDirectory k8sDirectory Dhall.defaultInputSettings)

loadK8sExample :: (String, String) -> IO (String, Dhall.InputSettings, ParsedExpr, ResolvedExpr)
loadK8sExample (name, expressionText) = do
    let sourceName = k8sDirectory </> name <> ".dhall"
    let settings = k8sSettings sourceName
    let prefix = "k8s/" <> name

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText sourceName (Text.pack expressionText))

    resolved <- timed (prefix <> ": resolve (cache on)") $
        resolveWithCache settings parsed

    timed (prefix <> ": typecheck") (ensureWellTyped resolved)
    say $ "  " <> prefix <> ": ready"

    pure (name, settings, parsed, resolved)

loadK8sExamples :: Maybe String -> IO [(String, Dhall.InputSettings, ParsedExpr, ResolvedExpr)]
loadK8sExamples mPattern = do
    let candidates =
            [ ( "file3", "(./file3.dhall).mkPod" )
            , ( "file4", "(./file4.dhall).mkPod" )
            ]
        selected =
            [ entry
            | entry@(name, _) <- candidates
            , any (couldMatch mPattern) (k8sLabels name)
            ]
    if null selected
        then do
            say "Skipping k8s fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading k8s fixtures (" <> show (length selected) <> " file(s))…"
            traverse loadK8sExample selected


parseLarge1 :: FilePath -> Text -> ParsedExpr
parseLarge1 path text =
    either throw id (Parser.exprFromText path text)

benchmarks :: Maybe String -> IO [Benchmark]
benchmarks mPattern = do
    examples <- loadExamples mPattern

    let wantLarge1 = any (couldMatch mPattern) large1Labels
    large1 <-
        if wantLarge1
            then Just <$> loadLarge1
            else do
                say "Skipping large1 (does not match pattern)"
                pure Nothing

    let wantLarge2 = any (couldMatch mPattern) large2Labels
    large2 <-
        if wantLarge2
            then Just <$> loadLarge2
            else do
                say "Skipping large2 (does not match pattern)"
                pure Nothing

    k8sExamples <- loadK8sExamples mPattern

    let wantLarge3 = any (couldMatch mPattern) large3Labels
    large3 <-
        if wantLarge3
            then Just <$> loadPipelineBench "large3" large3Directory "pipeline.dhall"
            else do
                say "Skipping large3 (does not match pattern)"
                pure Nothing

    let wantLarge3GetConfig = any (couldMatch mPattern) large3GetConfigLabels
    large3GetConfig <-
        if wantLarge3GetConfig
            then
                Just
                    <$> loadPipelineBench
                        "large3.get_config"
                        large3Directory
                        "get_config.dhall"
            else do
                say "Skipping large3.get_config (does not match pattern)"
                pure Nothing

    let wantLarge4 = any (couldMatch mPattern) large4Labels
    large4 <-
        if wantLarge4
            then Just <$> loadPipelineBench "large4" large4Directory "generate-example.dhall"
            else do
                say "Skipping large4 (does not match pattern)"
                pure Nothing

    let wantLarge5Code = any (couldMatch mPattern) large5CodeLabels
    large5Code <-
        if wantLarge5Code
            then
                Just
                    <$> loadPipelineBench
                        "large5"
                        large5Directory
                        "pipeline-code.dhall"
            else do
                say "Skipping large5 (does not match pattern)"
                pure Nothing

    large6Variants <- loadLarge6PhaseVariants mPattern
    large6ColdResolveVariants <- loadLarge6ColdResolveVariants mPattern

    let wantPreludeImportCode = any (couldMatch mPattern) preludeImportCodeLabels
    preludeImportCode <-
        if wantPreludeImportCode
            then
                Just
                    <$> loadColdResolveBench
                        "prelude_import"
                        preludeImportDirectory
                        "prelude-code.dhall"
            else do
                say "Skipping prelude_import (does not match pattern)"
                pure Nothing

    pure $ concat
        [ [ bgroup
              "normalize"
              [ bgroup
                  name
                  [ bench "typecheck" (nf typecheckResolvedExpr expression)
                  , bench "evaluation" (nf normalizeResolvedExpr expression)
                  ]
              | (name, expression) <- examples
              ]
          ]
        , [ bgroup "large1"
              [ bench "parse" (nf (parseLarge1 large1MainPath) large1Text)
              , bench "resolve" (nfAppIO (resolveWithoutCache large1Settings) large1Parsed)
              , bench "typecheck" (nf typecheckResolvedExpr large1Resolved)
              , bench "evaluation" (nf normalizeResolvedExpr large1Resolved)
              ]
          | Just (large1Text, large1Parsed, large1Resolved) <- [large1]
          ]
        , [ bgroup "large2"
              [ bench "normalize" (nf normalizeResolvedExpr large2Resolved)
              , bgroup
                    "cbor"
                    [ bench "encode" (nf encodeNormalized large2Normalized)
                    , bench "decode" (nf decodeNormalized large2Encoded)
                    ]
              ]
          | Just (large2Resolved, large2Normalized, large2Encoded) <- [large2]
          ]
        , [ bgroup
              "k8s"
              [ bgroup
                  name
                  [ bench "resolve" (nfAppIO (resolveWithoutCache settings) parsed)
                  , bench "typecheck" (nf typecheckResolvedExpr resolved)
                  , bench "evaluation" (nf normalizeResolvedExpr resolved)
                  ]
              | (name, settings, parsed, resolved) <- k8sExamples
              ]
          | not (null k8sExamples)
          ]
        , [ pipelineBenchGroup fixture | Just fixture <- [large3] ]
        , [ pipelineBenchGroup fixture | Just fixture <- [large3GetConfig] ]
        , [ pipelineBenchGroup fixture | Just fixture <- [large4] ]
        , [ pipelineBenchGroup fixture | Just fixture <- [large5Code] ]
        , map pipelineBenchGroup large6Variants
        , map coldResolveBenchGroup large6ColdResolveVariants
        , [ coldResolveBenchGroup fixture | Just fixture <- [preludeImportCode] ]
        ]
