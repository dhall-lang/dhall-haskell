{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypeCheck where

import Control.Exception (catch)
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))
import Dhall.Errors (ContextualError(..), ElabError(..))

import qualified Control.Monad     as Monad
import qualified Data.Text         as Text
import qualified Dhall.Eval        as Eval
import qualified Dhall.Elaboration as Elab
import qualified Dhall.Test.Util   as Test.Util

import qualified Test.Tasty        as Tasty
import qualified Test.Tasty.HUnit  as Tasty.HUnit
import qualified Turtle

typecheckDirectory :: FilePath
typecheckDirectory = "./dhall-lang/tests/typecheck"

getTests :: IO TestTree
getTests = do
    successTests <- Test.Util.discover
      (Turtle.chars <* "A.dhall")
      successTest
      (Turtle.lstree (typecheckDirectory </> "success"))

    let failureTestFiles = do
            path <- Turtle.lstree (typecheckDirectory </> "failure")
            let skip = []
            Monad.guard (path `notElem` skip)
            return path

    failureTests <- Test.Util.discover
      (Turtle.chars <> ".dhall")
      failureTest
      failureTestFiles

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
        actualExpr   <- Test.Util.getRaw actualCode
        expectedType <- Eval.evalEmpty <$> Test.Util.getCore expectedCode
        _ <- Elab.checkRoot "." actualExpr expectedType
        pure ()

failureTest :: Text -> TestTree
failureTest path = do
    Tasty.HUnit.testCase (Text.unpack path) $ do
        expression <- Test.Util.getRaw $ Test.Util.toDhallPath path

        typeChecked <- (True <$ Elab.inferRoot "." expression)
                       `catch`
                       \(ContextualError _ _ _ TypeError{}) -> pure False

        if typeChecked
            then fail (Text.unpack path <> " should not have type-checked")
            else pure ()
