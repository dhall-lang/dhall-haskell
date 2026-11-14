{-# LANGUAGE OverloadedStrings #-}
-- | Unique-node metrics for a hash-consed repeated tree.
--
-- This used to live on the semisemantic size-walk bench. That bench went
-- away with the tag-0 normal-form cache; the graph counters stayed.
module Bench.GraphMetrics
    ( benchmarks
    ) where

import Data.Void (Void)
import Test.Tasty.Bench

import Bench.Common (couldMatch)

import qualified Dhall.Core as Core
import qualified Dhall.Core.DAG as DAG

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
    ]

benchmarks :: Maybe String -> IO [Benchmark]
benchmarks mPattern =
    if any (couldMatch mPattern) graphMetricLabels
        then pure [graphMetricsBenchGroup]
        else pure []

graphMetricsBenchGroup :: Benchmark
graphMetricsBenchGroup =
    bgroup "graph.metrics"
        [ bench "unique_nodes"
            (nf (DAG.uniqueNodes . graphMetricsOfSharedTree) graphMetricsDepth)
        ]

graphMetricsOfSharedTree :: Int -> DAG.GraphMetrics
graphMetricsOfSharedTree depth =
    DAG.graphMetrics (DAG.fromExpr (sharedNaturalPlusTree depth))
