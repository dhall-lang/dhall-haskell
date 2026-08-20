{-# LANGUAGE OverloadedStrings #-}
-- | Shared types, resolve helpers, and Mode A/B/D bench builders for the
-- evaluation suite. See README.md for harness modes.
module Bench.Common
    ( ParsedExpr
    , ResolvedExpr
    , PipelineBench (..)
    , ColdResolveBench (..)
    , say
    , timed
    , patternFromArgs
    , couldMatch
    , phaseLabels
    , coldResolveBenchName
    , coldResolveLabels
    , endToEndColdBenchName
    , resolveWithCache
    , resolveWithoutCache
    , resolveWithColdCache
    , resolveWithSettingsCold
    , resolveTypecheckNormalizeCold
    , ensureWellTyped
    , pipelineSettings
    , loadPipelineBench
    , loadColdResolveBench
    , loadColdResolveBenchWithSettings
    , encodeNormalized
    , decodeNormalized
    , typecheckResolvedExpr
    , normalizeResolvedExpr
    , pipelineBenchGroup
    , coldResolveBenchGroup
    , substitutionsColdResolveBenchGroup
    , endToEndColdBenchGroup
    ) where

import Control.Exception (bracket, throw)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Void (Void)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Test.Tasty.Bench

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Binary as Binary
import qualified Dhall.Core as Core
import qualified Dhall.Import as Import
import qualified Dhall.Parser as Parser
import qualified Dhall.TypeCheck as TypeCheck
import qualified Lens.Micro
import Lens.Micro ((^.))
import qualified System.IO.Temp as Temp

type ParsedExpr = Core.Expr Parser.Src Core.Import
type ResolvedExpr = Core.Expr Parser.Src Void

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
-- Used for fixture preparation (Mode A) so broken fixtures fail early and
-- typecheck/evaluation benches have a resolved AST. See ../README.md.
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
-- @dhall --no-cache@). Semisemantic cache (@dhall-haskell-v2/@) is still read.
-- Used for Mode A timed @resolve@ benches.
resolveWithoutCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithoutCache settings parsed =
    Import.loadWithStatus
        ( Dhall.emptyStatusWithSettings
            (settings ^. Dhall.evaluateSettings)
            (settings ^. Dhall.rootDirectory)
        )
        Import.IgnoreSemanticCache
        parsed

-- | Mode B: resolve with caches on under a fresh @XDG_CACHE_HOME@ per sample.
resolveWithColdCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithColdCache settings parsed =
    withFreshCacheHome (resolveWithCache settings parsed)

-- | Mode B using the library substitution path ('Dhall.resolveWithSettings'),
-- matching @inputExprWithSettings@ rather than @Import.loadWithStatus@ alone.
resolveWithSettingsCold :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithSettingsCold settings parsed =
    withFreshCacheHome (Dhall.resolveWithSettings settings parsed)

-- | Mode D: cold library resolve, then typecheck and normalize under a fresh
-- cache. Synthetic substitution-heavy end-to-end path (import + typecheck + NF).
resolveTypecheckNormalizeCold
    :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveTypecheckNormalizeCold settings parsed =
    withFreshCacheHome $ do
        resolved <- Dhall.resolveWithSettings settings parsed
        _ <- either throw pure (TypeCheck.typeOf resolved)
        pure (Core.normalize resolved)

withFreshCacheHome :: IO a -> IO a
withFreshCacheHome action = do
    originalCacheHome <- lookupEnv "XDG_CACHE_HOME"

    Temp.withSystemTempDirectory "dhall-evaluation-bench" $ \cacheHome ->
        bracket
            (setEnv "XDG_CACHE_HOME" cacheHome)
            (\() -> restoreCacheHome originalCacheHome)
            (\() -> action)

restoreCacheHome :: Maybe String -> IO ()
restoreCacheHome =
    maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME")

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
    match ("-p", v)        = Just v
    match ("--pattern", v) = Just v
    match _                = Nothing

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

phaseLabels :: String -> [String]
phaseLabels prefix =
    [ prefix
    , prefix <> ".resolve"
    , prefix <> ".typecheck"
    , prefix <> ".evaluation"
    ]

-- | Bench name for Mode B cold resolve (see ../README.md).
coldResolveBenchName :: String
coldResolveBenchName = "resolve_cold_cache_on"

coldResolveLabels :: String -> [String]
coldResolveLabels prefix =
    [ prefix
    , prefix <> "." <> coldResolveBenchName
    ]

-- | Bench name for Mode D end-to-end cold (resolve + typecheck + normalize).
endToEndColdBenchName :: String
endToEndColdBenchName = "end_to_end_cold"

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
    -- ^ Mode A fixture: needs pre-resolved AST for typecheck/evaluation benches.

data ColdResolveBench = ColdResolveBench
    { crbGroupLabel :: String
    , crbSettings :: Dhall.InputSettings
    , crbParsed :: ParsedExpr
    }
    -- ^ Mode B fixture: parse-only prep; only cold resolve is measured.

pipelineSettings :: FilePath -> FilePath -> Dhall.InputSettings
pipelineSettings directory path =
    Lens.Micro.set Dhall.sourceName path
        (Lens.Micro.set Dhall.rootDirectory directory Dhall.defaultInputSettings)

-- | Mode A loader: parse, cache-warming resolve, validity typecheck.
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

-- | Mode B loader: parse only (no cache-warming resolve).
loadColdResolveBench :: String -> FilePath -> FilePath -> IO ColdResolveBench
loadColdResolveBench groupLabel directory relativePath =
    loadColdResolveBenchWithSettings groupLabel directory relativePath id

loadColdResolveBenchWithSettings
    :: String
    -> FilePath
    -> FilePath
    -> (Dhall.InputSettings -> Dhall.InputSettings)
    -> IO ColdResolveBench
loadColdResolveBenchWithSettings groupLabel directory relativePath tweakSettings = do
    let path = directory </> relativePath
    let prefix = groupLabel

    text <- timed (prefix <> ": read") (Text.IO.readFile path)

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText path text)

    let settings = tweakSettings (pipelineSettings directory path)

    say $ "  " <> prefix <> ": ready (explicit cold resolve benchmark)"

    pure
        ColdResolveBench
            { crbGroupLabel = groupLabel
            , crbSettings = settings
            , crbParsed = parsed
            }


typecheckResolvedExpr :: ResolvedExpr -> Core.Expr Parser.Src Void
typecheckResolvedExpr = either throw id . TypeCheck.typeOf

normalizeResolvedExpr :: ResolvedExpr -> ResolvedExpr
normalizeResolvedExpr = Core.normalize

pipelineBenchGroup :: PipelineBench -> Benchmark
pipelineBenchGroup fixture =
    bgroup (pbGroupLabel fixture)
        [ bench "resolve" (nfAppIO (resolveWithoutCache (pbSettings fixture)) (pbParsed fixture))
        , bench "typecheck" (nf typecheckResolvedExpr (pbResolved fixture))
        , bench "evaluation" (nf normalizeResolvedExpr (pbResolved fixture))
        ]

coldResolveBenchGroup :: ColdResolveBench -> Benchmark
coldResolveBenchGroup fixture =
    bgroup (crbGroupLabel fixture)
        [ bench coldResolveBenchName
            (nfAppIO (resolveWithColdCache (crbSettings fixture)) (crbParsed fixture))
        ]

substitutionsColdResolveBenchGroup :: ColdResolveBench -> Benchmark
substitutionsColdResolveBenchGroup fixture =
    bgroup (crbGroupLabel fixture)
        [ bench coldResolveBenchName
            (nfAppIO (resolveWithSettingsCold (crbSettings fixture)) (crbParsed fixture))
        ]

endToEndColdBenchGroup :: ColdResolveBench -> Benchmark
endToEndColdBenchGroup fixture =
    bgroup (crbGroupLabel fixture)
        [ bench endToEndColdBenchName
            (nfAppIO
                (resolveTypecheckNormalizeCold (crbSettings fixture))
                (crbParsed fixture)
            )
        ]
