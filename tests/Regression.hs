{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Regression where

import qualified Data.Map
import qualified Dhall
import qualified Dhall.Core
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Util

import Test.Tasty (TestTree)

regressionTests :: TestTree
regressionTests =
    Test.Tasty.testGroup "regression tests"
        [ issue96
          , unnamedFields
        ]

data Foo = Foo Integer Bool | Bar Bool Bool Bool | Baz Integer Integer
    deriving (Show, Dhall.Generic, Dhall.Interpret, Dhall.Inject)

unnamedFields :: TestTree
unnamedFields = Test.Tasty.HUnit.testCase "Unnamed Fields" (do
    let ty = Dhall.auto @Foo
    Test.Tasty.HUnit.assertEqual "Good type" (Dhall.expected ty) (Dhall.Core.Union (
            Data.Map.fromList [
                ("Bar",Dhall.Core.Record (Data.Map.fromList [
                    ("_1",Dhall.Core.Bool),("_2",Dhall.Core.Bool),("_3",Dhall.Core.Bool)]))
                , ("Baz",Dhall.Core.Record (Data.Map.fromList [
                    ("_1",Dhall.Core.Integer),("_2",Dhall.Core.Integer)]))
                ,("Foo",Dhall.Core.Record (Data.Map.fromList [
                    ("_1",Dhall.Core.Integer),("_2",Dhall.Core.Bool)]))]))

    let inj = Dhall.inject @Foo
    Test.Tasty.HUnit.assertEqual "Good Inject" (Dhall.declared inj) (Dhall.expected ty)
    return () )

issue96 :: TestTree
issue96 = Test.Tasty.HUnit.testCase "Issue #96" (do
    -- Verify that parsing should not fail
    _ <- Util.code "\"bar'baz\""
    return () )
