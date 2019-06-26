{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeInference where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)

import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text.IO
import qualified Dhall.Core        as Core
import qualified Dhall.Import      as Import
import qualified Dhall.Parser      as Parser
import qualified Dhall.Test.Util   as Test.Util
import qualified Dhall.TypeCheck   as TypeCheck
import qualified Test.Tasty        as Tasty
import qualified Test.Tasty.HUnit  as Tasty.HUnit
import qualified Turtle

getTests :: IO TestTree
getTests = do
    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest (Turtle.lstree "./dhall-lang/tests/type-inference/success")

    let testTree = Tasty.testGroup "type-inference tests"
            [ successTests
            ]

    return testTree

successTest :: Text -> TestTree
successTest prefix = do
    Tasty.HUnit.testCase prefixS $ do
        value <- expr (prefixS <> "A.dhall")

        expectedType <- expr (prefixS <> "B.dhall")

        inferredType <- Core.throws (TypeCheck.typeOf value)

        let message = "The inferred type did not match the expected type"

        Tasty.HUnit.assertEqual message expectedType inferredType
  where
    expr filepath = do
        code <- Text.IO.readFile filepath
        e <- Core.throws (Parser.exprFromText mempty code)
        Import.assertNoImports (Core.denote e)

    prefixS = Text.unpack prefix
