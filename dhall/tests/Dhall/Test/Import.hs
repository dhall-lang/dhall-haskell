{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Import where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Import (MissingImports(..))
import Dhall.Parser (SourcedException(..))
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Control.Exception                as Exception
import qualified Control.Monad                    as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified System.FilePath                  as FilePath
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.HUnit                 as Tasty.HUnit
import qualified Turtle

importDirectory :: FilePath
importDirectory = "./dhall-lang/tests/import"

getTests :: IO TestTree
getTests = do
    let flakyTests =
            [ -- This test is flaky, occasionally failing with:
              --
              --     Error: Remote host not found
              --
              --     URL: https://test.dhall-lang.org/Bool/package.dhall
              importDirectory </> "success/headerForwardingA.dhall"
            ]

    successTests <- Test.Util.discover (Turtle.chars <> "A.dhall") successTest (do
        path <- Turtle.lstree (importDirectory </> "success")

        Monad.guard (path `notElem` flakyTests)

        return path )

    failureTests <- Test.Util.discover (Turtle.chars <> ".dhall") failureTest (Turtle.lstree (importDirectory </> "failure"))

    let testTree =
            Tasty.testGroup "import tests"
                [ successTests
                , failureTests
                ]

    return testTree

successTest :: Text -> TestTree
successTest path = do
    let pathString = Text.unpack path

    let directoryString = FilePath.takeDirectory pathString

    Tasty.HUnit.testCase pathString (do

        text <- Text.IO.readFile pathString

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        let setCache =
                Turtle.export "XDG_CACHE_HOME" "dhall-lang/tests/import/cache"

        let unsetCache = Turtle.unset "XDG_CACHE_HOME"

        let setTestVar = Turtle.export "DHALL_TEST_VAR" "6 * 7"

        let unsetTestVar = Turtle.unset "DHALL_TEST_VAR"

        let load =
                State.evalStateT (Test.Util.loadWith actualExpr) (Import.emptyStatus directoryString)

        if Turtle.filename (Turtle.fromText path) == "hashFromCacheA.dhall"
            then do
                setCache
                _ <- load
                unsetCache
            else do
                setTestVar
                _ <- load
                unsetTestVar )

failureTest :: Text -> TestTree
failureTest path = do
    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString (do
        text <- Text.IO.readFile pathString

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        Exception.catch
          (do _ <- Test.Util.load actualExpr

              fail "Import should have failed, but it succeeds")
          (\(SourcedException _ (MissingImports _)) -> pure ()) )
