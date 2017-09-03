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
import Test.Tasty.HUnit ((@?=))

regressionTests :: TestTree
regressionTests =
    Test.Tasty.testGroup "regression tests"
        [ issue96
        , issue126
        , unnamedFields
        , trailingSpaceAfterStringLiterals
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

    let tu_ty = Dhall.auto @(Integer, Bool)
    Test.Tasty.HUnit.assertEqual "Auto Tuple" (Dhall.expected tu_ty) (Dhall.Core.Record (
            Data.Map.fromList [ ("_1",Dhall.Core.Integer),("_2",Dhall.Core.Bool) ]))

    let tu_in = Dhall.inject @(Integer, Bool)
    Test.Tasty.HUnit.assertEqual "Inj. Tuple" (Dhall.declared tu_in) (Dhall.expected tu_ty)

    return () )

issue96 :: TestTree
issue96 = Test.Tasty.HUnit.testCase "Issue #96" (do
    -- Verify that parsing should not fail
    _ <- Util.code "\"bar'baz\""
    return () )

issue126 :: TestTree
issue126 = Test.Tasty.HUnit.testCase "Issue #126" (do
    e <- Util.code
        "''\n\
        \  foo\n\
        \  bar\n\
        \''"
    Util.normalize' e @?= "\"foo\\nbar\\n\"" )

trailingSpaceAfterStringLiterals :: TestTree
trailingSpaceAfterStringLiterals =
    Test.Tasty.HUnit.testCase "Trailing space after string literals" (do
        -- Verify that string literals parse correctly with trailing space
        -- (Yes, I did get this wrong at some point)
        _ <- Util.code "(''ABC'' ++ \"DEF\" )"
        return () )
