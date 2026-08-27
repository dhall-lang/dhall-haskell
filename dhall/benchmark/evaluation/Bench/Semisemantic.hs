{-# LANGUAGE OverloadedStrings #-}
-- | Pure @semisemantic.nf_size_walk@ probe (full walk vs early abort).
module Bench.Semisemantic
    ( benchmarks
    ) where

import Control.Monad (foldM)
import Data.Maybe (isNothing)
import Data.Void (Void)
import Test.Tasty.Bench

import Bench.Common (couldMatch)

import qualified Data.ByteString as StrictByteString
import qualified Data.Functor.Const as FunctorConst
import qualified Data.Text as Text
import qualified Dhall.Core as Core

-- | Shared @x + x@ tree for @semisemantic.nf_size_walk@. Unique nodes =
-- depth+1; a full walk visits @2^(depth+1)-1@ constructors (no DAG memo).
-- Depth 20 is ~2M visits, well above the 64KiB store-NF cutoff.
-- 'NOINLINE' keeps GHC from constant-folding the size walk out of the bench.
{-# NOINLINE sharedNaturalPlusTree #-}
sharedNaturalPlusTree :: Int -> Core.Expr Void Void
sharedNaturalPlusTree depth = go depth
  where
    go 0 = Core.NaturalLit 0
    go n = let t = go (n - 1) in Core.NaturalPlus t t

nfSizeWalkDepth :: Int
nfSizeWalkDepth = 20

nfSizeWalkThreshold :: Int
nfSizeWalkThreshold = 64 * 1024

-- | Full-tree size estimate (naive baseline for @semisemantic.nf_size_walk.full@).
-- Same metric as 'nfSizeWalkExceedsThreshold'; kept in the harness so this
-- group does not depend on Import internals.
nfSizeWalkEstimate :: Core.Expr s a -> Int
nfSizeWalkEstimate expression =
    case expression of
        Core.TextLit (Core.Chunks xys z) ->
            1
          + Text.length z
          + sum [ Text.length t + nfSizeWalkEstimate x | (t, x) <- xys ]
        Core.BytesLit bytes ->
            1 + StrictByteString.length bytes
        Core.Embed _ ->
            1
        _ ->
            let FunctorConst.Const childSizes =
                    Core.subExpressions
                        (\child -> FunctorConst.Const [nfSizeWalkEstimate child])
                        expression
            in  1 + sum childSizes

-- | Early-abort size check (optimized side of @semisemantic.nf_size_walk@).
nfSizeWalkExceedsThreshold :: Core.Expr s a -> Bool
nfSizeWalkExceedsThreshold expression =
    isNothing (spend nfSizeWalkThreshold expression)
  where
    debit budget cost
        | budget < cost = Nothing
        | otherwise     = Just (budget - cost)

    spend budget _
        | budget <= 0 =
            Nothing
    spend budget (Core.TextLit (Core.Chunks xys z)) = do
        b0 <- debit budget (1 + Text.length z)
        foldM
            (\b (t, x) -> debit b (Text.length t) >>= (`spend` x))
            b0
            xys
    spend budget (Core.BytesLit bytes) =
        debit budget (1 + StrictByteString.length bytes)
    spend budget (Core.Embed _) =
        debit budget 1
    spend budget expr = do
        b0 <- debit budget 1
        let FunctorConst.Const kids =
                Core.subExpressions
                    (\child -> FunctorConst.Const [child])
                    expr
        foldM spend b0 kids

nfSizeWalkLabels :: [String]
nfSizeWalkLabels =
    [ "semisemantic.nf_size_walk"
    , "semisemantic.nf_size_walk.full"
    , "semisemantic.nf_size_walk.early_abort"
    ]



benchmarks :: Maybe String -> IO [Benchmark]
benchmarks mPattern =
    if any (couldMatch mPattern) nfSizeWalkLabels
        then pure [nfSizeWalkBenchGroup]
        else pure []

nfSizeWalkBenchGroup :: Benchmark
nfSizeWalkBenchGroup =
    bgroup "semisemantic.nf_size_walk"
        [ bench "full"
            (nf (nfSizeWalkEstimate . sharedNaturalPlusTree) nfSizeWalkDepth)
        , bench "early_abort"
            (nf (nfSizeWalkExceedsThreshold . sharedNaturalPlusTree) nfSizeWalkDepth)
        ]
