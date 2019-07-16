{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dhall.Test.TypeInference where

import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)

import qualified Data.Text         as Text
import qualified Dhall.Elaboration as Elab
import qualified Dhall.Eval        as Eval
import qualified Dhall.Test.Util   as Util
import qualified Test.Tasty        as Tasty
import qualified Test.Tasty.HUnit  as Tasty.HUnit

import qualified Turtle

getTests :: IO TestTree
getTests = do
    successTests <- Util.discover
      (Turtle.chars <* "A.dhall")
      successTest
      (Turtle.lstree "./dhall-lang/tests/type-inference/success")

    let testTree = Tasty.testGroup "type-inference tests"
            [ successTests ]

    return testTree

successTest :: Text -> TestTree
successTest prefix = do
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
      actualExpr <- Util.getRaw (prefix <> "A.dhall")
      expectedType <- Eval.evalEmpty <$> Util.getCore (prefix <> "B.dhall")
      _ <- Elab.checkRoot "." actualExpr expectedType
      pure ()
