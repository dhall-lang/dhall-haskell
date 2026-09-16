{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.DAG (tests) where

import Data.Void (Void)
import Dhall.Core
    ( Expr (..)
    , Var (..)
    , alphaNormalize
    , denote
    , makeFunctionBinding
    , shift
    , subst
    )
import Dhall.Core.DAG
    ( CoreGraph
    , GraphMetrics (..)
    , SharingMode (..)
    , alphaNormalizeGraph
    , compact
    , fromExpr
    , fromExprWith
    , graphMetrics
    , nodeCount
    , shiftGraph
    , substGraph
    , toSharedExpr
    , validate
    )
import Dhall.Pretty (ChooseCharacterSet (..))
import Test.Tasty   (TestTree)
import Test.Tasty.HUnit ((@?=))

import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as HUnit

tests :: TestTree
tests =
    Tasty.testGroup "core graph"
        [ HUnit.testCase "round-trip denoted trees" $ do
            let e = App (Var (V "f" 0)) (NaturalLit 1)
            toSharedExpr (fromPure e) @?= denote e

        , HUnit.testCase "sharing on/off same tree" $ do
            let e = repeatedPlus 8
            toSharedExpr (fromExprWith SharingOn e)
                @?= toSharedExpr (fromExprWith SharingOff e)

        , HUnit.testCase "repeated plus is O(unique nodes)" $ do
            let n = 12
                g = fromPure (repeatedPlus n)
            uniqueNodes (graphMetrics g) @?= n + 1
            case validate g of
                Left err -> HUnit.assertFailure err
                Right () -> pure ()

        , HUnit.testCase "sharing-off unique equals expanded" $ do
            let n = 8
                g = fromExprWith SharingOff (repeatedPlus n)
            uniqueNodes (graphMetrics g) @?= 2 ^ (n + 1) - 1

        , HUnit.testCase "child-before-parent" $ do
            case validate (fromPure (repeatedPlus 6)) of
                Left err -> HUnit.assertFailure err
                Right () -> pure ()

        , HUnit.testCase "compact remaps a reachable DAG" $ do
            let g = compact (fromPure (repeatedPlus 5))
            toSharedExpr g @?= denote (repeatedPlus 5)
            case validate g of
                Left err -> HUnit.assertFailure err
                Right () -> pure ()

        , HUnit.testCase "distinct nodes are not merged" $ do
            let g = fromPure (NaturalPlus (NaturalLit 0) (NaturalLit 1))
            nodeCount g @?= 3

        , HUnit.testCase "shadowed open syntax may share NodeId" $ do
            -- λ(x : Natural) → λ(x : Natural) → x@1 + x
            let body = NaturalPlus (Var (V "x" 1)) (Var (V "x" 0))
                e = Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
                    (Lam AutoInferCharSet (makeFunctionBinding "x" Natural) body)
                g = fromPure e
            uniqueNodes (graphMetrics g) @?= 6

        , HUnit.testCase "shift parity" $ do
            let e = Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
                        (Var (V "y" 0))
                v = V "y" 0
            toSharedExpr (shiftGraph 1 v (fromPure e))
                @?= denote (shift 1 v e)

        , HUnit.testCase "subst parity" $ do
            let e = Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
                        (Var (V "y" 0))
                r = Var (V "x" 0) :: Expr Void Void
            toSharedExpr (substGraph (V "y" 0) (fromPure r) (fromPure e))
                @?= denote (subst (V "y" 0) r e)

        , HUnit.testCase "alpha-normalize parity" $ do
            let e = Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
                        (Var (V "x" 0))
            toSharedExpr (alphaNormalizeGraph (fromPure e))
                @?= denote (alphaNormalize e)

        , HUnit.testCase "intern hits on shared tree" $ do
            let on  = fromPure (repeatedPlus 4)
                off = fromExprWith SharingOff (repeatedPlus 4)
            internMisses (graphMetrics on) @?= 5
            internHits (graphMetrics on) > 0 @?= True
            internHits (graphMetrics off) @?= 0
            internMisses (graphMetrics off) @?= fromIntegral (uniqueNodes (graphMetrics off))
        ]

fromPure :: Expr Void Void -> CoreGraph Void
fromPure = fromExpr

-- Unique nodes = depth+1; expanded occurrences = 2^(depth+1)-1.
repeatedPlus :: Int -> Expr Void Void
repeatedPlus depth = go depth
  where
    go 0 = NaturalLit 0
    go n = let t = go (n - 1) in NaturalPlus t t
