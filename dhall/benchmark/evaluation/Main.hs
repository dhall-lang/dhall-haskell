{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception  (throw)
import Data.Char         (toLower)
import Data.List         (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe        (listToMaybe, mapMaybe)
import Data.Text         (Text)
import Data.Time.Clock   (diffUTCTime, getCurrentTime)
import Data.Void         (Void)
import System.Environment (getArgs)
import System.FilePath   ((</>), takeBaseName, takeDirectory)
import System.IO         (hFlush, stdout)
import Text.Printf       (printf)
import Test.Tasty.Bench

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall
import qualified Dhall.Binary         as Binary
import qualified Dhall.Core           as Core
import qualified Dhall.Import         as Import
import qualified Dhall.Parser         as Parser
import qualified Dhall.TypeCheck      as TypeCheck
import qualified Lens.Micro
import           Lens.Micro           ((^.))
import qualified System.Directory     as Directory

type ParsedExpr = Core.Expr Parser.Src Core.Import
type ResolvedExpr = Core.Expr Parser.Src Void

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

large3PipelinePath :: FilePath
large3PipelinePath = large3Directory </> "pipeline.dhall"

large4Directory :: FilePath
large4Directory = "benchmark/evaluation/large4"

large4PipelinePath :: FilePath
large4PipelinePath = large4Directory </> "generate-example.dhall"

large5Directory :: FilePath
large5Directory = "benchmark/evaluation/large5"

large5CodePipelinePath :: FilePath
large5CodePipelinePath = large5Directory </> "pipeline-code.dhall"

large5SourcePipelinePath :: FilePath
large5SourcePipelinePath = large5Directory </> "pipeline-source.dhall"

large6Directory :: FilePath
large6Directory = "benchmark/evaluation/large6"

k8sDirectory :: FilePath
k8sDirectory = "benchmark/evaluation/k8s"

say :: String -> IO ()
say msg = putStrLn msg >> hFlush stdout

formatDuration :: Double -> String
formatDuration seconds
    | seconds < 0.001 = printf "%.0f μs" (seconds * 1e6)
    | seconds < 1     = printf "%.1f ms" (seconds * 1e3)
    | otherwise       = printf "%.2f s" seconds

-- | Informational wall-clock timing for preparation steps (not a Criterion bench).
timed :: String -> IO a -> IO a
timed label action = do
    say $ "  " <> label <> "…"
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let seconds = realToFrac (diffUTCTime end start) :: Double
    say $ "  " <> label <> ": " <> formatDuration seconds
    pure result

-- | Resolve imports using the normal semantic / semisemantic disk caches.
-- Used only for fixture preparation so large imports (e.g. large1) are not
-- paid cold on every bench run.
resolveWithCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithCache settings parsed =
    Import.loadWithStatus
        ( Dhall.emptyStatusWithSettings
            (settings ^. Dhall.evaluateSettings)
            (settings ^. Dhall.rootDirectory)
        )
        Import.UseSemanticCache
        parsed

-- | Resolve imports with the semantic disk cache disabled (as with
-- @dhall --no-cache@).  Used for the timed @resolve@ benches.
resolveWithoutCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithoutCache settings parsed =
    Import.loadWithStatus
        ( Dhall.emptyStatusWithSettings
            (settings ^. Dhall.evaluateSettings)
            (settings ^. Dhall.rootDirectory)
        )
        Import.IgnoreSemanticCache
        parsed

-- | Abort if the expression is ill-typed.  Used at load time so fixtures never
-- report OK timings when type-checking fails.
ensureWellTyped :: ResolvedExpr -> IO ()
ensureWellTyped expression =
    either throw (\_ -> pure ()) (TypeCheck.typeOf expression)

-- | Best-effort extract of @-p@ / @--pattern@ from the benchmark argv, so we can
-- skip loading fixtures that tasty-bench would filter out anyway.
patternFromArgs :: [String] -> Maybe String
patternFromArgs args =
    listToMaybe $ mapMaybe match (zip args (drop 1 args)) ++ naked
  where
    match ("-p", value)        = Just value
    match ("--pattern", value) = Just value
    match _                    = Nothing

    naked =
        [ drop (length prefix) arg
        | arg <- args
        , prefix <- ["--pattern=", "-p="]
        , prefix `isPrefixOf` arg
        ]

-- | Whether a tasty-style label path could match the user pattern (infix, like
-- tasty's default).  @Nothing@ means no pattern → load everything.
couldMatch :: Maybe String -> String -> Bool
couldMatch Nothing _ = True
couldMatch (Just pat) label =
    map toLower pat `isInfixOf` map toLower label

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

phaseLabels :: String -> [String]
phaseLabels prefix =
    [ prefix
    , prefix <> ".resolve"
    , prefix <> ".typecheck"
    , prefix <> ".evaluation"
    ]

large3Labels :: [String]
large3Labels = phaseLabels "large3"

large4Labels :: [String]
large4Labels = phaseLabels "large4"

large5CodeLabels :: [String]
large5CodeLabels = phaseLabels "large5.code"

large5SourceLabels :: [String]
large5SourceLabels = phaseLabels "large5.source"

large6Labels :: [String]
large6Labels =
    "large6"
        : concat
            [ phaseLabels ("large6." <> variant)
            | variant <-
                [ "slow_parse.as_code"
                , "slow_parse.as_source"
                , "slow_eval.as_code"
                , "slow_eval.as_source"
                , "slow_typecheck.as_code"
                , "slow_typecheck.as_source"
                ]
            ]

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

encodeNormalized :: ResolvedExpr -> ByteString.ByteString
encodeNormalized = Binary.encodeExpression . Core.denote

decodeNormalized :: ByteString.ByteString -> Core.Expr Void Void
decodeNormalized =
    either throw id . Binary.decodeExpression

data PipelineBench = PipelineBench
    { pbGroupLabel :: String
    , pbSettings :: Dhall.InputSettings
    , pbParsed :: ParsedExpr
    , pbResolved :: ResolvedExpr
    }

pipelineSettings :: FilePath -> FilePath -> Dhall.InputSettings
pipelineSettings directory path =
    Lens.Micro.set Dhall.sourceName path
        (Lens.Micro.set Dhall.rootDirectory directory Dhall.defaultInputSettings)

-- | Load a Dhall pipeline for resolve / typecheck / evaluation benches.
loadPipelineBench :: String -> FilePath -> FilePath -> IO PipelineBench
loadPipelineBench groupLabel directory relativePath = do
    let path = directory </> relativePath
    let prefix = groupLabel

    text <- timed (prefix <> ": read") (Text.IO.readFile path)

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText path text)

    let settings = pipelineSettings directory path

    resolved <- timed (prefix <> ": resolve (cache on)") $
        resolveWithCache settings parsed

    timed (prefix <> ": typecheck") (ensureWellTyped resolved)
    say $ "  " <> prefix <> ": ready"

    pure
        PipelineBench
            { pbGroupLabel = groupLabel
            , pbSettings = settings
            , pbParsed = parsed
            , pbResolved = resolved
            }

loadLarge6Variants :: Maybe String -> IO [PipelineBench]
loadLarge6Variants mPattern = do
    let candidates =
            [ ("large6.slow_parse.as_code", "pipeline-code-long-parse.dhall")
            , ("large6.slow_parse.as_source", "pipeline-source-long-parse.dhall")
            , ("large6.slow_eval.as_code", "pipeline-code-long-eval.dhall")
            , ("large6.slow_eval.as_source", "pipeline-source-long-eval.dhall")
            , ("large6.slow_typecheck.as_code", "pipeline-code-long-typecheck.dhall")
            , ("large6.slow_typecheck.as_source", "pipeline-source-long-typecheck.dhall")
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
            say $ "Loading large6 fixtures (" <> show (length selected) <> " file(s))…"
            traverse (\(label, file) -> loadPipelineBench label large6Directory file) selected

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

-- Prep uses the normal disk caches; timed @resolve@ benches still use
-- 'resolveWithoutCache'.
main :: IO ()
main = do
    args <- getArgs
    let mPattern = patternFromArgs args
    case mPattern of
        Nothing  -> say "Preparing benchmarks (loading fixtures; prep cache on)…"
        Just pat -> say $ "Preparing benchmarks matching " <> show pat <> " (prep cache on)…"

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
                        "large5.code"
                        large5Directory
                        "pipeline-code.dhall"
            else do
                say "Skipping large5.code (does not match pattern)"
                pure Nothing

    let wantLarge5Source = any (couldMatch mPattern) large5SourceLabels
    large5Source <-
        if wantLarge5Source
            then
                Just
                    <$> loadPipelineBench
                        "large5.source"
                        large5Directory
                        "pipeline-source.dhall"
            else do
                say "Skipping large5.source (does not match pattern)"
                pure Nothing

    large6Variants <- loadLarge6Variants mPattern

    say "Starting tasty-bench…"

    defaultMain $ concat
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
        , [ pipelineBenchGroup large3Bench
          | Just large3Bench <- [large3]
          ]
        , [ pipelineBenchGroup large4Bench
          | Just large4Bench <- [large4]
          ]
        , [ pipelineBenchGroup large5CodeBench
          | Just large5CodeBench <- [large5Code]
          ]
        , [ pipelineBenchGroup large5SourceBench
          | Just large5SourceBench <- [large5Source]
          ]
        , map pipelineBenchGroup large6Variants
        ]
 where
   -- These helpers reduce polymorphism in TypeCheck.typeOf and Core.normalize.
   -- Type-check failures must throw so tasty-bench reports FAIL instead of OK.
   typecheckResolvedExpr :: ResolvedExpr -> Core.Expr Parser.Src Void
   typecheckResolvedExpr = either throw id . TypeCheck.typeOf

   -- Pure NbE; disk caches are irrelevant here. Ill-typed fixtures are rejected
   -- at load time via 'ensureWellTyped'.
   normalizeResolvedExpr :: ResolvedExpr -> ResolvedExpr
   normalizeResolvedExpr = Core.normalize

   pipelineBenchGroup :: PipelineBench -> Benchmark
   pipelineBenchGroup fixture =
       bgroup (pbGroupLabel fixture)
           [ bench "resolve" (nfAppIO (resolveWithoutCache (pbSettings fixture)) (pbParsed fixture))
           , bench "typecheck" (nf typecheckResolvedExpr (pbResolved fixture))
           , bench "evaluation" (nf normalizeResolvedExpr (pbResolved fixture))
           ]

   parseLarge1 :: FilePath -> Text -> ParsedExpr
   parseLarge1 path text =
       either throw id (Parser.exprFromText path text)
