module Main where

import Control.Exception (throw)
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
    , "large1.cbor"
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

-- | Load large1 inputs for per-phase benchmarks.
--
-- Import resolution for prep uses 'resolveWithCache'. A one-time normalize for
-- the CBOR fixture runs here so the timed @typecheck@ / @evaluation@ / @cbor@
-- benches start from the right artifact. The timed @resolve@ bench re-runs
-- resolution with 'resolveWithoutCache' via 'nfAppIO'.
loadLarge1 :: IO (Text, ParsedExpr, ResolvedExpr, ResolvedExpr)
loadLarge1 = do
    say "Loading large1…"

    text <- timed "large1: read" (Text.IO.readFile large1MainPath)

    parsed <- timed "large1: parse" $
        either throw pure (Parser.exprFromText large1MainPath text)

    resolved <- timed "large1: resolve (cache on)" $
        resolveWithCache large1Settings parsed

    timed "large1: typecheck" (ensureWellTyped resolved)

    -- Lazy: prep time may under-report if work is deferred until the cbor bench.
    normalized <- timed "large1: normalize (for cbor fixture)" $
        pure (Core.normalize resolved)

    say "  large1: ready"

    pure (text, parsed, resolved, normalized)

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

    k8sExamples <- loadK8sExamples mPattern

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
              , bench "cbor" (nf encodeNormalized large1Normalized)
              ]
          | Just (large1Text, large1Parsed, large1Resolved, large1Normalized) <- [large1]
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

   parseLarge1 :: FilePath -> Text -> ParsedExpr
   parseLarge1 path text =
       either throw id (Parser.exprFromText path text)

   encodeNormalized :: ResolvedExpr -> ByteString.ByteString
   encodeNormalized = Binary.encodeExpression . Core.denote
