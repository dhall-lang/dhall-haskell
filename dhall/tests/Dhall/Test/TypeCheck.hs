{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeCheck where

import Control.Exception (SomeException)
import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Control.Exception as Exception
import qualified Data.Text         as Text
import qualified Dhall.Core        as Core
import qualified Dhall.Parser      as Parser
import qualified Dhall.Test.Util   as Test.Util
import qualified Dhall.TypeCheck   as TypeCheck
import qualified Test.Tasty        as Tasty
import qualified Test.Tasty.HUnit  as Tasty.HUnit
import qualified Turtle

typecheckDirectory :: FilePath
typecheckDirectory = "./dhall-lang/tests/typecheck"

getTests :: IO TestTree
getTests = do
    let successTestFiles = Turtle.lstree (typecheckDirectory </> "success")

    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest successTestFiles

    let failureTestFiles = Turtle.lstree (typecheckDirectory </> "failure")

    failureTests <- Test.Util.discover (Turtle.chars <> ".dhall") failureTest failureTestFiles

    let testTree = Tasty.testGroup "typecheck tests"
            [ successTests
            , failureTests
            ]

    return testTree

successTest :: Text -> TestTree
successTest prefix = do
    let skip = [ typecheckDirectory </> "success/preferMixedRecords"
               , typecheckDirectory </> "success/preferMixedRecordsSameField"
               , typecheckDirectory </> "success/prelude" -- fixed in dhall-lang/dhall-lang#708
               , typecheckDirectory </> "success/RecordTypeMixedKinds"
               , typecheckDirectory </> "success/simple/combineMixedRecords"
               , typecheckDirectory </> "success/simple/RecordMixedKinds2"
               , typecheckDirectory </> "success/simple/RecordMixedKinds"
               , typecheckDirectory </> "success/simple/RecursiveRecordMergeMixedKinds"
               , typecheckDirectory </> "success/simple/RightBiasedRecordMergeMixedKinds"
               ]

    Test.Util.testCase prefix skip $ do
        let actualCode   = Test.Util.toDhallPath (prefix <> "A.dhall")
        let expectedCode = Test.Util.toDhallPath (prefix <> "B.dhall")

        actualExpr   <- Core.throws (Parser.exprFromText mempty actualCode  )
        expectedExpr <- Core.throws (Parser.exprFromText mempty expectedCode)

        let annotatedExpr = Core.Annot actualExpr expectedExpr

        tryResolvedExpr <- Exception.try (Test.Util.load annotatedExpr)

        resolvedExpr <- case tryResolvedExpr of
            Left  exception    -> Tasty.HUnit.assertFailure (show (exception :: SomeException))
            Right resolvedExpr -> return resolvedExpr

        case TypeCheck.typeOf resolvedExpr of
            Left  exception -> Tasty.HUnit.assertFailure (show exception)
            Right _         -> return ()
{-
        _ <- Core.throws (TypeCheck.typeOf resolvedExpr)
-}

        return ()

failureTest :: Text -> TestTree
failureTest path = do
    let skip = [ typecheckDirectory </> "failure/unit/MergeEmptyNeedsDirectAnnotation1.dhall"
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
