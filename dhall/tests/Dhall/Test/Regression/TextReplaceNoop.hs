{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Test.Regression.TextReplaceNoop (tests) where

import Data.Text              (Text)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       ((@?=))

import qualified Dhall
import qualified Test.Tasty
import qualified Test.Tasty.HUnit


tests :: TestTree
tests =
    Test.Tasty.testGroup "TextReplaceNoop"
        [ Test.Tasty.HUnit.testCase "Text replacement should not become a no-op when the input contains append" $ do

            fn :: Text -> Text
                <- Dhall.input Dhall.auto "\\(x : Text) -> Text/replace \".\" \"!\" (x ++ \"!\")"

            fn "..."
                @?= "!!!!"

        , Test.Tasty.HUnit.testCase "Text replacement should match a needle spanning a chunk boundary" $ do

            -- Once `name` is substituted, the haystack ("X" ++ name ++ "X")
            -- resolves to the literal "XclassX", which fully contains the
            -- needle "XclassX" -- but only once the "X" prefix, the
            -- substituted interpolation, and the "X" suffix are considered
            -- together. `Text/replace` currently matches the needle against
            -- each chunk in isolation (before re-joining them), so a needle
            -- that only exists once the chunks are concatenated is never
            -- found and the replacement silently becomes a no-op.
            fn :: Text -> Text
                <- Dhall.input Dhall.auto "\\(name : Text) -> Text/replace \"XclassX\" \"XREPLACEDX\" (\"X\" ++ name ++ \"X\")"

            fn "class"
                @?= "XREPLACEDX"
        ]

