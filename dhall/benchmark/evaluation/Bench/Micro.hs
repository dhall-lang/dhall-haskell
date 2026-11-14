{-# LANGUAGE OverloadedStrings #-}
-- | Small evaluator shapes: a beta chain, closure allocation, a deep 'let'
-- chain, and a tree walk against graph intern of one shared normal form.
--
-- Omitted from the default suite. Pass a pattern containing @micro@.
module Bench.Micro
    ( benchmarks
    ) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Void (Void)
import Test.Tasty.Bench

import Bench.Common (couldMatch, say)

import qualified Dhall.Core as Core
import qualified Dhall.Pretty as Pretty

type E = Core.Expr Void Void

wantsMicro :: Maybe String -> Bool
wantsMicro Nothing = False
wantsMicro (Just pat) = "micro" `isInfixOf` map toLower pat

nat :: E
nat = Core.Natural

var :: Text -> E
var name = Core.Var (Core.V name 0)

lam :: Text -> E -> E -> E
lam name ty body =
    Core.Lam Pretty.AutoInferCharSet (Core.makeFunctionBinding name ty) body

-- | @n@ applications of @\(x : Natural) -> x@ to @0@.
betaChain :: Int -> E
betaChain n = go n
  where
    ident = lam "x" nat (var "x")
    go 0 = Core.NaturalLit 0
    go k = Core.App ident (go (k - 1))

-- | @n@ nested lambdas. Normalizing them allocates a closure per binder.
nestedLams :: Int -> E
nestedLams n = go n
  where
    go 0 = var "x"
    go k = lam "x" nat (go (k - 1))

-- | @n@ nested lets. Each one extends the environment.
nestedLets :: Int -> E
nestedLets n = go n
  where
    go 0 = var "x"
    go k = Core.Let (Core.makeBinding "x" (Core.NaturalLit 0)) (go (k - 1))

-- | Physically shared @x + x@ tree. A tree fold visits @2^(depth+1) - 1@
-- constructors; interning sees @depth + 1@ nodes.
sharedPlus :: Int -> E
sharedPlus depth = go depth
  where
    go 0 = var "x"
    go n = let t = go (n - 1) in Core.NaturalPlus t t

treeNodes :: E -> Int
treeNodes = go
  where
    go (Core.NaturalPlus a b) = 1 + go a + go b
    go (Core.Var _) = 1
    go _ = 1

graphNodes :: E -> Int
graphNodes = Core.uniqueNodes . Core.graphMetrics . Core.fromExpr

normalizeE :: E -> E
normalizeE = Core.normalize

quoteDepth :: Int
quoteDepth = 16

betaCount :: Int
betaCount = 20000

nestCount :: Int
nestCount = 4000

labels :: [String]
labels =
    [ "micro.beta"
    , "micro.closures"
    , "micro.extend"
    , "micro.quote.tree"
    , "micro.quote.graph"
    ]

benchmarks :: Maybe String -> IO [Benchmark]
benchmarks mPattern
    | not (wantsMicro mPattern) = do
        say "Skipping micro fixtures (pass a pattern containing micro)"
        pure []
    | not (any (couldMatch mPattern) labels) = do
        say "Skipping micro fixtures (do not match pattern)"
        pure []
    | otherwise = do
        say "Loading micro fixtures…"
        let plus = sharedPlus quoteDepth
        pure
            [ bgroup "micro"
                [ bench "beta" (nf normalizeE (betaChain betaCount))
                , bench "closures" (nf normalizeE (nestedLams nestCount))
                , bench "extend" (nf normalizeE (nestedLets nestCount))
                , bench "quote.tree" (nf treeNodes plus)
                , bench "quote.graph" (nf graphNodes plus)
                ]
            ]
