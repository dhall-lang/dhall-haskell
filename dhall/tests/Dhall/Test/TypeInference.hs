{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeInference where

import Control.Exception (SomeException)
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

    failureTests <- Test.Util.discover (Turtle.chars <* "A.dhall") failureTest failureTestFiles

    let testTree = Tasty.testGroup "type-inference tests"
            [ successTests
            , failureTests
            ]

    return testTree

successTest :: Text -> TestTree
successTest prefix = do
    let skip = [ -- We correctly infer the expected type @NaN ≡ NaN@ here,
                 -- but the comparison between the inferred and the expected type
                 -- fails due to `Expr`'s 'Eq' instance, which inherits the
                 -- @NaN /= NaN@ inequality from 'Double'.
                 typeInferenceDirectory </> "success/unit/AssertNaN"
               ]

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
failureTest path = do
    let skip = [ typeInferenceDirectory </> "failure/unit/MergeEmptyNeedsDirectAnnotation1.dhall"
               ]

    Test.Util.testCase path skip $ do
        let dhallPath = Test.Util.toDhallPath path

        expression <- Core.throws (Parser.exprFromText mempty dhallPath)

        let io :: IO Bool
            io = do
                _ <- Test.Util.load expression
                return True

        let handler :: SomeException -> IO Bool
            handler _ = return False

        typeChecked <- Exception.handle handler io

        if typeChecked
            then Tasty.HUnit.assertFailure (Text.unpack path <> " should not have type-checked")
            else return ()
