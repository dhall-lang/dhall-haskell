{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Test.Regression.TextReplaceNoop (tests) where

import Data.Text              (Text)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       ((@?=))

import qualified Dhall
import qualified Test.Tasty.HUnit


tests :: TestTree
tests =
    Test.Tasty.HUnit.testCase "Text replacement should not be a no-op" $ do

        fn :: [Text] -> Text
            <- Dhall.input Dhall.auto "./tests/Dhall/Test/Regression/TextReplaceNoop.dhall"
            
        fn ["..."]
            @?= "\"!!!\""

