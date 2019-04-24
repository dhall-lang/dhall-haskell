{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeCheck where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Dhall.Import (Imported)
import Dhall.Parser (Src)
import Dhall.TypeCheck (TypeError, X)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Control.Exception as Exception
import qualified Control.Monad     as Monad
import qualified Data.Text         as Text
import qualified Dhall.Core        as Core
import qualified Dhall.Import      as Import
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
    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest (Turtle.lstree (typecheckDirectory </> "success"))

    let failureTestFiles = do
            path <- Turtle.lstree (typecheckDirectory </> "failure")

            let skip =
                    [ typecheckDirectory </> "failure/duplicateFields.dhall"
                    ]

            Monad.guard (path `notElem` skip)

            return path

    failureTests <- Test.Util.discover (Turtle.chars <> ".dhall") failureTest failureTestFiles

    let testTree = Tasty.testGroup "typecheck tests"
            [ successTests
            , failureTests
            ]

    return testTree

successTest :: Text -> TestTree
successTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let actualCode   = Test.Util.toDhallPath (prefix <> "A.dhall")
        let expectedCode = Test.Util.toDhallPath (prefix <> "B.dhall")

        actualExpr <- Core.throws (Parser.exprFromText mempty actualCode)

        expectedExpr <- Core.throws (Parser.exprFromText mempty expectedCode)

        let annotatedExpr = Core.Annot actualExpr expectedExpr

        resolvedExpr <- Import.load annotatedExpr

        _ <- Core.throws (TypeCheck.typeOf resolvedExpr)

        return ()

failureTest :: Text -> TestTree
failureTest path = do
    Tasty.HUnit.testCase (Text.unpack path) $ do
        let dhallPath = Test.Util.toDhallPath path

        expression <- Core.throws (Parser.exprFromText mempty dhallPath)

        let io :: IO Bool
            io = do
                _ <- Import.load expression
                return True

        let handler :: Imported (TypeError Src X)-> IO Bool
            handler _ = return False

        typeChecked <- Exception.handle handler io

        if typeChecked
            then fail (Text.unpack path <> " should not have type-checked")
            else return ()
