module Main where

import Control.Exception (throw)
import Data.List         (isSuffixOf, sort)
import Data.Text         (Text)
import Data.Void         (Void)
import System.FilePath   ((</>), takeBaseName, takeDirectory)
import Test.Tasty.Bench

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.IO         as Text
import qualified Dhall
import qualified Dhall.Binary         as Binary
import qualified Dhall.Core           as Core
import qualified Dhall.Parser         as Parser
import qualified Dhall.TypeCheck      as TypeCheck
import qualified Lens.Micro
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

loadExamples :: IO [(String, ResolvedExpr)]
loadExamples = do
    files <- sort <$> Directory.listDirectory normalizeDirectory
    traverse loadExample
        [ normalizeDirectory </> file | file <- files, ".dhall" `isSuffixOf` file ]
  where
    normalizeDirectory = "benchmark/evaluation/normalize"

loadExample :: FilePath -> IO (String, ResolvedExpr)
loadExample path = do
    text <- Text.readFile path

    parsed <-
        either throw pure (Parser.exprFromText path text)

    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)

    resolved <- Dhall.resolveWithSettings settings parsed

    pure (takeBaseName path, resolved)

-- | Load large1 inputs for per-phase benchmarks.
--
-- Import resolution (and a one-time normalize for the CBOR fixture) run here
-- so the timed @typecheck@ / @evaluation@ / @cbor@ benches start from the
-- right artifact.  The @resolve@ bench re-runs resolution on the parsed
-- expression via 'nfAppIO'.
loadLarge1 :: IO (Text, ParsedExpr, ResolvedExpr, ResolvedExpr)
loadLarge1 = do
    text <- Text.readFile large1MainPath

    parsed <-
        either throw pure (Parser.exprFromText large1MainPath text)

    resolved <- Dhall.resolveWithSettings large1Settings parsed

    let normalized = Core.normalize resolved

    pure (text, parsed, resolved, normalized)

main :: IO ()
main = do
    examples <- loadExamples
    (large1Text, large1Parsed, large1Resolved, large1Normalized) <- loadLarge1

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
            , bench "resolve" (nfAppIO (Dhall.resolveWithSettings large1Settings) large1Parsed)
            , bench "typecheck" (nf typecheckResolvedExpr large1Resolved)
            , bench "evaluation" (nf normalizeResolvedExpr large1Resolved)
            , bench "cbor" (nf encodeNormalized large1Normalized)
            ]
        ]
 where
   -- These helpers are needed just to reduce polymorphism in TypeCheck.typeOf and Core.normalize.
   typecheckResolvedExpr :: ResolvedExpr -> Maybe (Core.Expr Parser.Src Void)
   typecheckResolvedExpr = either (const Nothing) Just . TypeCheck.typeOf

   normalizeResolvedExpr :: ResolvedExpr -> ResolvedExpr
   normalizeResolvedExpr = Core.normalize

   parseLarge1 :: FilePath -> Text -> ParsedExpr
   parseLarge1 path text =
       either throw id (Parser.exprFromText path text)

   encodeNormalized :: ResolvedExpr -> ByteString.ByteString
   encodeNormalized = Binary.encodeExpression . Core.denote
