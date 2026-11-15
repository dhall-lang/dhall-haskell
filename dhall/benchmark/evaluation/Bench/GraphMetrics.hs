{-# LANGUAGE OverloadedStrings #-}
-- | Unique-node metrics for a hash-consed repeated tree.
--
-- This used to live on the semisemantic size-walk bench. That bench went
-- away with the tag-0 normal-form cache; the graph counters stayed.
module Bench.GraphMetrics
    ( benchmarks
    ) where

import Control.Exception (throw)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Void (Void)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty.Bench

import Bench.Common (ResolvedExpr, couldMatch, ensureWellTyped, resolveWithCache, say, timed)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Binary as Binary
import qualified Dhall.Core as Core
import qualified Dhall.Core.DAG as DAG
import qualified Dhall.Core.DAG.CBOR as DAGCBOR
import qualified Dhall.Import as Import
import qualified Dhall.Parser as Parser
import qualified Lens.Micro as Lens

-- | Shared @x + x@ tree. Sharing-on intern uses @depth + 1@ unique nodes.
-- A tree fold would visit @2^(depth+1) - 1@ constructors.
{-# NOINLINE sharedNaturalPlusTree #-}
sharedNaturalPlusTree :: Int -> Core.Expr Void Void
sharedNaturalPlusTree depth = go depth
  where
    go 0 = Core.NaturalLit 0
    go n = let t = go (n - 1) in Core.NaturalPlus t t

graphMetricsDepth :: Int
graphMetricsDepth = 20

graphMetricLabels :: [String]
graphMetricLabels =
    [ "graph.metrics"
    , "graph.metrics.unique_nodes"
    , "graph.metrics.hash_tree"
    , "graph.metrics.hash_graph"
    , "graph.metrics.cbor_tree"
    , "graph.metrics.cbor_graph"
    ]

sharedTree :: Core.Expr Void Void
sharedTree = sharedNaturalPlusTree graphMetricsDepth

benchmarks :: Maybe String -> IO [Benchmark]
benchmarks mPattern = do
    synthetic <-
        if any (couldMatch mPattern) graphMetricLabels
            then do
                let graph = DAG.fromExpr sharedTree
                    treeBytes = Lazy.length (Binary.encodeExpression sharedTree)
                    graphBytes = Lazy.length (Lazy.fromStrict (DAGCBOR.encodeGraph graph))
                say $ "graph.metrics depth " <> show graphMetricsDepth
                    <> " unique " <> show (DAG.uniqueNodes (DAG.graphMetrics graph))
                    <> " tree_bytes " <> show treeBytes
                    <> " graph_bytes " <> show graphBytes
                pure [graphMetricsBenchGroup]
            else pure []
    dagout <- dagoutBenchmarks mPattern
    pure (synthetic ++ dagout)

graphMetricsBenchGroup :: Benchmark
graphMetricsBenchGroup =
    bgroup "graph.metrics"
        [ bench "unique_nodes"
            (nf (DAG.uniqueNodes . graphMetricsOfSharedTree) graphMetricsDepth)
        , bench "hash_tree" (nf Import.hashExpression sharedTree)
        , bench "hash_graph" (nf (Core.hashExpressionGraph . DAG.fromExpr) sharedTree)
        , bench "hash_legacy" (nf (DAGCBOR.hashGraphLegacy . DAG.fromExpr) sharedTree)
        , bench "present_bytes"
            (nf (Text.length . Core.pretty . Core.presentationExpr . Core.CanonicalGraph . DAG.fromExpr) sharedTree)
        , bench "full_bytes" (nf (Text.length . Core.pretty) sharedTree)
        , bench "cbor_tree" (nf (Lazy.length . Binary.encodeExpression) sharedTree)
        , bench "cbor_graph"
            (nf (Lazy.length . Lazy.fromStrict . DAGCBOR.encodeGraph . DAG.fromExpr) sharedTree)
        ]

-- | Real normal forms. Opt-in: a pattern containing @dagout@. Prep prints
-- unique nodes and both encodings once; the benches repeat the work.
dagoutBenchmarks :: Maybe String -> IO [Benchmark]
dagoutBenchmarks mPattern
    | not (wantsDagout mPattern) = pure []
    | otherwise = do
        large1 <-
            if any (couldMatch mPattern) (dagoutRowLabels "dagout.large1")
                then (:[]) <$> loadDagout "dagout.large1" loadLarge1
                else pure []
        k8s <-
            if any (couldMatch mPattern) (dagoutRowLabels "dagout.k8s.file3")
                then (:[]) <$> loadDagout "dagout.k8s.file3" loadK8sFile3
                else pure []
        pure (large1 ++ k8s)
  where
    wantsDagout Nothing = False
    wantsDagout (Just pat) = "dagout" `isInfixOf` map toLower pat

dagoutRowLabels :: String -> [String]
dagoutRowLabels prefix =
    [ prefix <> ".hash_tree"
    , prefix <> ".hash_from_expr"
    , prefix <> ".cbor_tree"
    , prefix <> ".cbor_graph"
    ]

loadDagout :: String -> IO ResolvedExpr -> IO Benchmark
loadDagout label load = do
    resolved <- load
    normal <- timed (label <> ": normalize") $
        pure (Core.denote (Core.normalize resolved))
    let graph = DAG.fromExpr normal
        treeBytes = Lazy.length (Binary.encodeExpression normal)
        graphBytes = Lazy.length (Lazy.fromStrict (DAGCBOR.encodeGraph graph))
    say $ label
        <> " unique " <> show (DAG.uniqueNodes (DAG.graphMetrics graph))
        <> " tree_bytes " <> show treeBytes
        <> " graph_bytes " <> show graphBytes
    pure $ bgroup label
        [ bench "hash_tree" (nf Import.hashExpression normal)
        , bench "hash_from_expr" (nf (Core.hashExpressionGraph . DAG.fromExpr) normal)
        , bench "cbor_tree" (nf (Lazy.length . Binary.encodeExpression) normal)
        , bench "cbor_graph"
            (nf (Lazy.length . Lazy.fromStrict . DAGCBOR.encodeGraph . DAG.fromExpr) normal)
        ]

loadLarge1 :: IO ResolvedExpr
loadLarge1 = do
    let path = "benchmark" </> "evaluation" </> "large1" </> "main.dhall"
    text <- timed "dagout/large1: read" (Text.IO.readFile path)
    parsed <- timed "dagout/large1: parse" $
        either throw pure (Parser.exprFromText path text)
    let settings =
            Lens.set Dhall.sourceName path
                (Lens.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)
    resolved <- timed "dagout/large1: resolve" $
        resolveWithCache settings parsed
    timed "dagout/large1: typecheck" (ensureWellTyped resolved)
    pure resolved

loadK8sFile3 :: IO ResolvedExpr
loadK8sFile3 = do
    let directory = "benchmark" </> "evaluation" </> "k8s"
        sourceName = directory </> "file3.dhall"
        settings =
            Lens.set Dhall.sourceName sourceName
                (Lens.set Dhall.rootDirectory directory Dhall.defaultInputSettings)
    parsed <- timed "dagout/k8s.file3: parse" $
        either throw pure (Parser.exprFromText sourceName "(./file3.dhall).mkPod")
    resolved <- timed "dagout/k8s.file3: resolve" $
        resolveWithCache settings parsed
    timed "dagout/k8s.file3: typecheck" (ensureWellTyped resolved)
    pure resolved

graphMetricsOfSharedTree :: Int -> DAG.GraphMetrics
graphMetricsOfSharedTree depth =
    DAG.graphMetrics (DAG.fromExpr (sharedNaturalPlusTree depth))
