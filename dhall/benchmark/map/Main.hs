{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main (defaultMain, bgroup, bench, whnf, nfIO)

import qualified Criterion.Main as Criterion
import qualified Dhall.Map as Map

testData :: Integer -> Map.Map Integer Integer
testData i = foldr (\j -> Map.insert j j) mempty [1 .. i]

benchOrderedTraversal :: String -> Map.Map Integer Integer -> Criterion.Benchmark
benchOrderedTraversal dataLabel mapData =
    bgroup ("Ordered Traversals: " <> dataLabel)
        [ bench "traverseWithKey" $
            whnf (Map.traverseWithKey (\_ i -> pure @Maybe $ i ^ i)) mapData
        , bench "traverseWithKey_" $
            whnf (Map.traverseWithKey_ (\_ i -> pure @Maybe (i ^ i) *> pure ())) mapData
        ]

benchUnorderedTraversal :: String -> Map.Map Integer Integer -> Criterion.Benchmark
benchUnorderedTraversal dataLabel mapData =
    bgroup ("Unordered Traversals: " <> dataLabel)
        [ bench "unorderedTraverseWithKey_" $
            whnf (Map.unorderedTraverseWithKey_ (\_ i -> pure @Maybe (i ^ i) *> pure ())) mapData
        ]

main :: IO ()
main = do
    let !smallMap  = testData 10
        !mediumMap = testData 1000
        !largeMap  = testData 100000
    defaultMain
        [ benchOrderedTraversal   "small" smallMap
        , benchUnorderedTraversal "small" smallMap

        , benchOrderedTraversal   "medium" mediumMap
        , benchUnorderedTraversal "medium" mediumMap

        , benchOrderedTraversal   "large"  largeMap
        , benchUnorderedTraversal "large"  largeMap
        ]
