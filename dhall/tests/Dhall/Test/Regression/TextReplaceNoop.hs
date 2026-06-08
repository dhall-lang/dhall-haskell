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
    Test.Tasty.HUnit.testCase "Text replacement should not become a no-op when the input contains append" $ do

        fn :: Text -> Text
            <- Dhall.input Dhall.auto "\\(x : Text) -> Text/replace \".\" \"!\" (x ++ \"!\")"
            
        fn "..."
            @?= "!!!!"

