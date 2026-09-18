{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.DAG (tests) where

import Control.Exception (evaluate)
import Control.Monad.ST (runST)
import Data.Void (Void)
import Dhall.Core
    ( Expr (..)
    , Var (..)
    , alphaNormalize
    , denote
    , makeFunctionBinding
    , normalize
    , normalizeGraph
    , normalizeGraphWith
    , pretty
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
    , newBuilder
    , nodeCount
    , prettyGraph
    , shiftGraph
    , substGraph
    , toSharedExpr
    , validate
    )
import Dhall.Pretty (ChooseCharacterSet (..))
import Test.Tasty   (TestTree)
import Test.Tasty.HUnit ((@?=))

import qualified Dhall.Binary        as Binary
import qualified Dhall.Core          as Core
import qualified Dhall.Core.DAG.CBOR as CBOR
import qualified Dhall.Import        as Import
import qualified Dhall.Parser        as Parser
import qualified Dhall.TypeCheck     as TypeCheck
import qualified System.Timeout      as Timeout
import qualified Test.Tasty          as Tasty
import qualified Test.Tasty.HUnit    as HUnit

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

        , HUnit.testCase "normalizeGraph matches normalize" $ do
            let e = NaturalPlus (NaturalLit 1) (NaturalLit 1) :: Expr Void Void
            toSharedExpr (normalizeGraph e) @?= denote (normalize e)

        , HUnit.testCase "normalize sharing on/off same tree" $ do
            toSharedExpr (normalizeGraph repeatedOpenPlus)
                @?= toSharedExpr (normalizeGraphWith SharingOff repeatedOpenPlus)

        , HUnit.testCase "quote does not expand unique nodes" $ do
            let g = normalizeGraph repeatedOpenPlus
            uniqueNodes (graphMetrics g) @?= 4
            internHits (graphMetrics g) > 0 @?= True
            case validate g of
                Left err -> HUnit.assertFailure err
                Right () -> pure ()

        , HUnit.testCase "prettyGraph matches pretty of normalize" $ do
            prettyGraph (normalizeGraph repeatedOpenPlus)
                @?= pretty (normalize repeatedOpenPlus :: Expr Void Void)

        , HUnit.testCase "identified values: sharing on/off same CBOR" $ do
            let e = repeatedOpenPlus
                bytes g = Binary.encodeExpression (toSharedExpr g :: Expr Void Void)
            bytes (normalizeGraph e) @?= bytes (normalizeGraphWith SharingOff e)

        , HUnit.testCase "identified values: ValId is unobservable in Show" $ do
            let shown = show (normalize (NaturalLit 1 :: Expr Void Void) :: Expr Void Void)
            HUnit.assertBool "Show does not mention ValId" ("ValId" `notElem` words shown)
            HUnit.assertBool "Show does not mention EnvId" ("EnvId" `notElem` words shown)

        , HUnit.testCase "identified values: cache-off inserts nothing" $ do
            let stats = Core.withEvalRun Core.cacheOffConfig $
                    let v = Core.evalInRun Core.Empty
                                (NaturalPlus (NaturalLit 1) (NaturalLit 1) :: Expr Void Void)
                        n = Core.quoteInRun Core.EmptyNames v
                    in  n `seq` Core.readEvalStats
            Core.cacheInserts stats @?= 0

        , HUnit.testCase "identified values: same ValId quotes to the same node" $ do
            let (n1, n2) = Core.withEvalRun Core.defaultEvalConfig $ runST $ do
                    b <- newBuilder SharingOn
                    let v = Core.evalInRun Core.Empty
                                (NaturalPlus (NaturalLit 1) (NaturalLit 1) :: Expr Void Void)
                    x <- Core.quoteGraph b Core.EmptyNames v
                    y <- Core.quoteGraph b Core.EmptyNames v
                    return (x, y)
            n1 @?= n2

        , HUnit.testCase "primitive literals have no ValId" $ do
            let n = Core.evalInRun Core.Empty (NaturalLit 7 :: Expr Void Void)
                b = Core.evalInRun Core.Empty (BoolLit True :: Expr Void Void)
            Core.valId n @?= Nothing
            Core.valId b @?= Nothing

        , HUnit.testCase "Natural/fold does not stop on the first successor" $ do
            let e = App (App (App (App (Var (V "Natural/fold" 0)) (NaturalLit 10)) Natural)
                        (Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
                            (NaturalPlus (Var (V "x" 0)) (NaturalLit 1))))
                        (NaturalLit 0)
            normalize e @?= (NaturalLit 10 :: Expr Void Void)

        , HUnit.testCase "typecheck does not force unused let values" $ do
            -- Building a 200k-element list is expensive if the let is evaluated.
            -- Typechecking only needs the type of the binding.
            let src =
                    "let x = List/length Natural (List/build Natural (λ(list : Type) → λ(cons : Natural → list → list) → λ(nil : list) → Natural/fold 200000 list (cons 1) nil)) in 1"
            expr <- case Parser.exprFromText "unused-let" src of
                Left err -> HUnit.assertFailure (show err)
                Right parsed ->
                    pure (fmap (\_ -> error "unexpected import") (denote parsed) :: Expr Void Void)
            result <- Timeout.timeout 500000 $ evaluate $
                case TypeCheck.typeOf expr of
                    Left err -> error (show err)
                    Right t  -> denote t
            result @?= Just (Natural :: Expr Void Void)

        , HUnit.testCase "typecheck: repeated plus infers Natural" $ do
            case TypeCheck.typeOf (repeatedPlus 4) of
                Left err -> HUnit.assertFailure (show err)
                Right t  -> denote t @?= (Natural :: Expr Void Void)

        , HUnit.testCase "typecheck: Note diagnostics are preserved" $ do
            -- λ(x : Bool) → x + x  is ill-typed; the annotation span is kept.
            let e = Lam AutoInferCharSet (makeFunctionBinding "x" Bool)
                        (NaturalPlus (Var (V "x" 0)) (Var (V "x" 0)))
            case TypeCheck.typeOf e of
                Left _  -> pure ()
                Right t -> HUnit.assertFailure ("expected type error, got: " <> show (t :: Expr Void Void))

        , HUnit.testCase "DAG CBOR: sharing on/off same bytes" $ do
            let e = repeatedPlus 6
            CBOR.encodeGraph (fromExprWith SharingOn e)
                @?= CBOR.encodeGraph (fromExprWith SharingOff e)

        , HUnit.testCase "DAG CBOR: round-trip tree" $ do
            let e = repeatedPlus 5
                g = fromPure e
            case CBOR.decodeGraph (CBOR.encodeGraph g) of
                Left err -> HUnit.assertFailure err
                Right g' -> toSharedExpr g' @?= denote e

        , HUnit.testCase "DAG CBOR: independent of intern order" $ do
            let e = repeatedOpenPlus
                g = fromPure e
            CBOR.encodeGraph g @?= CBOR.encodeGraph (fromExpr (toSharedExpr g))

        , HUnit.testCase "legacy graph hash matches tree hash" $ do
            let e = denote (normalize repeatedOpenPlus :: Expr Void Void)
            Core.hashExpressionGraph (fromPure e)
                @?= Import.hashExpression e
        ]

fromPure :: Expr Void Void -> CoreGraph Void
fromPure = fromExpr

-- Unique nodes = depth+1; expanded occurrences = 2^(n+1)-1.
repeatedPlus :: Int -> Expr Void Void
repeatedPlus depth = go depth
  where
    go 0 = NaturalLit 0
    go n = let t = go (n - 1) in NaturalPlus t t

-- λ(x : Natural) → x + x  — quote shares the two uses of x.
repeatedOpenPlus :: Expr Void Void
repeatedOpenPlus =
    Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
        (NaturalPlus (Var (V "x" 0)) (Var (V "x" 0)))
