{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeInference where

import Control.Exception (SomeException (..))
import Data.Text         (Text)
import Prelude           hiding (FilePath)
import Test.Tasty        (TestTree)
import Turtle            (FilePath, (</>))

import qualified Control.Exception as Exception
import qualified Control.Monad     as Monad
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
    let successTestFiles = do
            path <- Turtle.lstree (typeInferenceDirectory </> "success")

            let skip =
                    -- These tests intermittently fails with:
                    -- "Error: Remote host not found"
                    [ typeInferenceDirectory </> "success/CacheImportsA.dhall"
                    , typeInferenceDirectory </> "success/CacheImportsCanonicalizeA.dhall"
                    ]

            Monad.guard (path `notElem` skip)

            return path

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
    let expectedFailures =
                []
#ifdef WITH_HTTP
#else
            ++  [ typeInferenceDirectory </> "success/CacheImports"
                ]
#endif

    Test.Util.testCase prefix expectedFailures $ do
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

        -- We also add this to exercise the `Dhall.Eval.conv` code path, since
        -- it's easy to forget to update it when adding new syntax
        _ <- Core.throws (TypeCheck.typeOf (Core.Annot resolvedExpr resolvedExpectedType))
        return ()

failureTest :: Text -> TestTree
failureTest prefix = do
    let expectedFailures =
               [
               -- Duplicate fields are incorrectly caught during parsing:
               -- https://github.com/dhall-lang/dhall-haskell/issues/772
                 typeInferenceDirectory </> "failure/unit/RecordTypeDuplicateFields"
               , typeInferenceDirectory </> "failure/unit/UnionTypeDuplicateVariants1"
               , typeInferenceDirectory </> "failure/unit/UnionTypeDuplicateVariants2"
               ]

    Test.Util.testCase prefix expectedFailures $ do
        let prefixFP = Text.unpack prefix

        code <- Text.IO.readFile (prefixFP <> ".dhall")

        expression <- case Parser.exprFromText mempty code of
            Left _ -> Tasty.HUnit.assertFailure (prefixFP <> " should have parsed")
            Right e -> return e

        resolved <- Import.assertNoImports expression

        case TypeCheck.typeOf resolved of
            Left  _ -> return ()
            Right _ -> Tasty.HUnit.assertFailure (prefixFP <> " should not have type-checked")
