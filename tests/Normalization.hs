{-# LANGUAGE OverloadedStrings #-}
module Normalization (normalizationTests) where

import           Dhall.Core
import           Test.Tasty
import           Test.Tasty.HUnit

normalizationTests :: TestTree
normalizationTests = testGroup "normalization" [ constantFolding ]

constantFolding :: TestTree
constantFolding = testGroup "folding of constants" [ naturalPlus ]

naturalPlus :: TestTree
naturalPlus = testCase "natural plus" $ normalize' (NaturalPlus (NaturalLit 1) (NaturalLit 2)) @?= NaturalLit 3

normalize' :: Expr () () -> Expr () ()
normalize' = normalize
