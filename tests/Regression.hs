{-# LANGUAGE OverloadedStrings #-}

module Regression where

import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Util

import Test.Tasty (TestTree)

regressionTests :: TestTree
regressionTests =
    Test.Tasty.testGroup "regression tests"
        [ issue96
        ]

issue96 :: TestTree
issue96 = Test.Tasty.HUnit.testCase "Issue #96" (do
    -- Verify that parsing should not fail
    _ <- Util.code "\"bar'baz\""
    return () )
