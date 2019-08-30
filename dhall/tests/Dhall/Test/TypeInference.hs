{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeInference where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

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

typeInferenceDirectory :: FilePath
typeInferenceDirectory = "./dhall-lang/tests/type-inference"

getTests :: IO TestTree
getTests = do
    let successTestFiles = Turtle.lstree (typeInferenceDirectory </> "success")

    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest successTestFiles

    let testTree = Tasty.testGroup "type-inference tests"
            [ successTests
            ]

    return testTree

successTest :: Text -> TestTree
successTest prefix = do
    let skip = [ -- We correctly infer the expected type @NaN ≡ NaN@ here,
                 -- but the comparison between the inferred and the expected type
                 -- fails due to `Expr`'s 'Eq' instance, which inherits the
                 -- @NaN /= NaN@ inequality from 'Double'.
                 typeInferenceDirectory </> "success/unit/AssertNaN"
               , typeInferenceDirectory </> "success/unit/RecursiveRecordMergeBoolType"
               , typeInferenceDirectory </> "success/simple/RecordTypeMixedKinds2"
               , typeInferenceDirectory </> "success/simple/RecordTypeMixedKinds3"
               ]

    Test.Util.testCase prefix skip $ do
        value <- expr "A.dhall"

        expectedType <- expr "B.dhall"

        inferredType <- case TypeCheck.typeOf value of
            Left  exception    -> Tasty.HUnit.assertFailure (show exception)
            Right inferredType -> return inferredType

        let message = "The inferred type did not match the expected type"

        Tasty.HUnit.assertEqual message expectedType inferredType
  where
    expr suffix = do
        code <- Text.IO.readFile (Text.unpack prefix <> suffix)

        e <- Core.throws (Parser.exprFromText mempty code)

        Import.assertNoImports (Core.denote e)
