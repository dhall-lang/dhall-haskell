{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeInference where

import Control.Exception (SomeException(..))
import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Control.Exception as Exception
import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text.IO
import qualified Dhall.Core        as Core
import qualified Dhall.Import      as Import
import qualified Dhall.Parser      as Parser
import qualified Dhall.Test.Util   as Test.Util
import qualified Dhall.TypeCheck   as TypeCheck
import qualified System.FilePath   as FilePath
import qualified Test.Tasty        as Tasty
import qualified Test.Tasty.HUnit  as Tasty.HUnit
import qualified Turtle

typeInferenceDirectory :: FilePath
typeInferenceDirectory = "./dhall-lang/tests/type-inference"

getTests :: IO TestTree
getTests = do
    let successTestFiles = Turtle.lstree (typeInferenceDirectory </> "success")

    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest successTestFiles

    let failureTestFiles = Turtle.lstree (typeInferenceDirectory </> "failure")

    failureTests <- Test.Util.discover (Turtle.chars <* ".dhall") failureTest failureTestFiles

    let testTree = Tasty.testGroup "type-inference tests"
            [ successTests
            , failureTests
            ]

    return testTree

successTest :: Text -> TestTree
successTest prefix = do
    let skip = []

    Test.Util.testCase prefix skip $ do
        let prefixFP = Text.unpack prefix

        actualCode <- Text.IO.readFile (prefixFP <> "A.dhall")

        actualExpr <- Core.throws (Parser.exprFromText mempty actualCode)

        tryResolvedExpr <-
            Exception.try
               (Test.Util.loadRelativeTo
                   (FilePath.takeDirectory prefixFP)
                   Import.IgnoreSemanticCache
                   (Core.denote actualExpr))

        resolvedExpr <- case tryResolvedExpr of
            Left  exception    -> Tasty.HUnit.assertFailure (show (exception :: SomeException))
            Right resolvedExpr -> return resolvedExpr

        expectedTypeCode <- Text.IO.readFile (prefixFP <> "B.dhall")

        expectedType <- Core.throws (Parser.exprFromText mempty expectedTypeCode)

        resolvedExpectedType <- Import.assertNoImports (Core.denote expectedType)

        inferredType <- case TypeCheck.typeOf resolvedExpr of
            Left  exception    -> Tasty.HUnit.assertFailure (show exception)
            Right inferredType -> return inferredType

        let message = "The inferred type did not match the expected type"

        Tasty.HUnit.assertEqual message resolvedExpectedType inferredType

failureTest :: Text -> TestTree
failureTest prefix = do
    let skip = [ typeInferenceDirectory </> "failure/unit/MergeEmptyNeedsDirectAnnotation1"

               -- Duplicate fields are incorrectly caught during parsing:
               -- https://github.com/dhall-lang/dhall-haskell/issues/772
               , typeInferenceDirectory </> "failure/unit/RecordLitDuplicateFields"
               , typeInferenceDirectory </> "failure/unit/RecordProjectionDuplicateFields"
               , typeInferenceDirectory </> "failure/unit/RecordTypeDuplicateFields"
               , typeInferenceDirectory </> "failure/unit/UnionTypeDuplicateVariants1"
               , typeInferenceDirectory </> "failure/unit/UnionTypeDuplicateVariants2"
               ]

    Test.Util.testCase prefix skip $ do
        let prefixFP = Text.unpack prefix

        code <- Text.IO.readFile (prefixFP <> ".dhall")

        expression <- case Parser.exprFromText mempty code of
            Left _ -> Tasty.HUnit.assertFailure (prefixFP <> " should have parsed")
            Right e -> return e

        resolved <- Import.assertNoImports expression

        case TypeCheck.typeOf resolved of
            Left  _ -> return ()
            Right _ -> Tasty.HUnit.assertFailure (prefixFP <> " should not have type-checked")
