module Main where

import Control.Exception (throw)
import Data.List         (isSuffixOf, sort)
import Data.Text         (Text)
import Data.Void         (Void)
import System.FilePath   ((</>), takeBaseName, takeDirectory)
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
import qualified System.Environment   as Environment
import           System.IO            (hClose, openTempFile)

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

-- | Resolve imports with the semantic disk cache disabled (as with
-- @dhall --no-cache@).  Combined with an empty @XDG_CACHE_HOME@ set in 'main',
-- the semisemantic disk cache is also unused.
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

loadExamples :: IO [(String, ResolvedExpr)]
loadExamples = do
    files <- sort <$> Directory.listDirectory normalizeDirectory
    traverse loadExample
        [ normalizeDirectory </> file | file <- files, ".dhall" `isSuffixOf` file ]
  where
    normalizeDirectory = "benchmark/evaluation/normalize"

loadExample :: FilePath -> IO (String, ResolvedExpr)
loadExample path = do
    text <- Text.IO.readFile path

    parsed <-
        either throw pure (Parser.exprFromText path text)

    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)

    resolved <- resolveWithoutCache settings parsed

    ensureWellTyped resolved

    pure (takeBaseName path, resolved)

-- | Load large1 inputs for per-phase benchmarks.
--
-- Import resolution uses 'resolveWithoutCache'. A one-time normalize for the
-- CBOR fixture runs here so the timed @typecheck@ / @evaluation@ / @cbor@
-- benches start from the right artifact. The @resolve@ bench re-runs resolution
-- on the parsed expression via 'nfAppIO'.
loadLarge1 :: IO (Text, ParsedExpr, ResolvedExpr, ResolvedExpr)
loadLarge1 = do
    text <- Text.IO.readFile large1MainPath

    parsed <-
        either throw pure (Parser.exprFromText large1MainPath text)

    resolved <- resolveWithoutCache large1Settings parsed

    ensureWellTyped resolved

    let normalized = Core.normalize resolved

    pure (text, parsed, resolved, normalized)

k8sSettings :: FilePath -> Dhall.InputSettings
k8sSettings sourceName =
    Lens.Micro.set Dhall.sourceName sourceName
        (Lens.Micro.set Dhall.rootDirectory k8sDirectory Dhall.defaultInputSettings)

loadK8sExample :: (String, String) -> IO (String, Dhall.InputSettings, ParsedExpr, ResolvedExpr)
loadK8sExample (name, expressionText) = do
    let sourceName = k8sDirectory </> name <> ".dhall"
    let settings = k8sSettings sourceName

    parsed <-
        either throw pure (Parser.exprFromText sourceName (Text.pack expressionText))

    resolved <- resolveWithoutCache settings parsed

    ensureWellTyped resolved

    pure (name, settings, parsed, resolved)

loadK8sExamples :: IO [(String, Dhall.InputSettings, ParsedExpr, ResolvedExpr)]
loadK8sExamples =
    traverse loadK8sExample
        [ ( "file3", "(./file3.dhall).mkPod" )
        , ( "file4", "(./file4.dhall).mkPod" )
        ]

-- | Point @XDG_CACHE_HOME@ at a fresh empty directory so Dhall's semantic and
-- semisemantic disk caches cannot serve or pollute the user's real cache.
withEmptyDhallCacheHome :: IO a -> IO a
withEmptyDhallCacheHome action = do
    tmp <- Directory.getTemporaryDirectory
    (path, handle) <- openTempFile tmp "dhall-evaluation-bench"
    hClose handle
    Directory.removeFile path
    let cacheHome = path <> ".xdg"
    Directory.createDirectory cacheHome
    Environment.setEnv "XDG_CACHE_HOME" cacheHome
    action

main :: IO ()
main = withEmptyDhallCacheHome $ do
    examples <- loadExamples
    (large1Text, large1Parsed, large1Resolved, large1Normalized) <- loadLarge1
    k8sExamples <- loadK8sExamples

    defaultMain
        [ bgroup
            "normalize"
            [ bgroup
                name
                [ bench "typecheck" (nf typecheckResolvedExpr expression)
                , bench "evaluation" (nf normalizeResolvedExpr expression)
                ]
            | (name, expression) <- examples
            ]
        , bgroup "large1"
            [ bench "parse" (nf (parseLarge1 large1MainPath) large1Text)
            , bench "resolve" (nfAppIO (resolveWithoutCache large1Settings) large1Parsed)
            , bench "typecheck" (nf typecheckResolvedExpr large1Resolved)
            , bench "evaluation" (nf normalizeResolvedExpr large1Resolved)
            , bench "cbor" (nf encodeNormalized large1Normalized)
            ]
        , bgroup
            "k8s"
            [ bgroup
                name
                [ bench "resolve" (nfAppIO (resolveWithoutCache settings) parsed)
                , bench "typecheck" (nf typecheckResolvedExpr resolved)
                , bench "evaluation" (nf normalizeResolvedExpr resolved)
                ]
            | (name, settings, parsed, resolved) <- k8sExamples
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
