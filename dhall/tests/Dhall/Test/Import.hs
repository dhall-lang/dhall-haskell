{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Dhall.Test.Import where

import Control.Exception (SomeException)
import Data.Text         (Text)
import Prelude           hiding (FilePath)
import Test.Tasty        (TestTree)
import Turtle            (FilePath, (</>))

import qualified Control.Exception                as Exception
import qualified Control.Monad                    as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified Network.HTTP.Client              as HTTP
import qualified Network.HTTP.Client.TLS          as HTTP
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
            , importDirectory </> "success/unit/RemoteAsTextA.dhall"
            , importDirectory </> "success/unit/SimpleRemoteA.dhall"
            , importDirectory </> "success/unit/asLocation/RemoteChain1A.dhall"
            , importDirectory </> "success/unit/asLocation/RemoteChain2A.dhall"
            , importDirectory </> "success/unit/asLocation/RemoteChain3A.dhall"
            , importDirectory </> "success/unit/asLocation/RemoteChainMissingA.dhall"
            ]

    successTests <- Test.Util.discover (Turtle.chars <> "A.dhall") successTest (do
        path <- Turtle.lstree (importDirectory </> "success")

        Monad.guard (path `notElem` flakyTests)

        return path )

    failureTests <- Test.Util.discover (Turtle.chars <> ".dhall") failureTest (do
        path <- Turtle.lstree (importDirectory </> "failure")

        let expectedSuccesses =
                [ importDirectory </> "failure/unit/DontRecoverCycle.dhall"
                , importDirectory </> "failure/unit/DontRecoverTypeError.dhall"
                ]

        Monad.guard (path `notElem` expectedSuccesses)
        return path )

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

    let expectedFailures =
            [ importDirectory </> "success/unit/cors/TwoHopsA.dhall"
            , importDirectory </> "success/unit/cors/SelfImportAbsoluteA.dhall"
            , importDirectory </> "success/unit/cors/AllowedAllA.dhall"
            , importDirectory </> "success/unit/cors/SelfImportRelativeA.dhall"
            , importDirectory </> "success/unit/cors/OnlyGithubA.dhall"
            ]

    Test.Util.testCase path expectedFailures (do

        text <- Text.IO.readFile pathString

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        let originalCache = "dhall-lang/tests/import/cache"

        let httpManager =
                HTTP.newManager
                    HTTP.tlsManagerSettings
                        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (120 * 1000 * 1000) }
        let load =
                State.evalStateT
                    (Test.Util.loadWith actualExpr)
                    (Import.emptyStatusWithManager httpManager directoryString)

        let usesCache = [ "hashFromCacheA.dhall"
                        , "unit/asLocation/HashA.dhall"
                        , "unit/IgnorePoisonedCacheA.dhall"
                        , "unit/DontCacheIfHashA.dhall"
                        ]

        let endsIn path' = not $ null $ Turtle.match (Turtle.ends path') path

        let buildNewCache = do
                tempdir <- Turtle.mktempdir "/tmp" "dhall-cache"
                Turtle.liftIO (Turtle.cptree originalCache tempdir)
                return tempdir

        let runTest =
                if any endsIn usesCache
                    then Turtle.runManaged $ do
                        cacheDir <- buildNewCache

                        let set = do
                                m <- Turtle.need "XDG_CACHE_HOME"

                                Turtle.export "XDG_CACHE_HOME" (Turtle.format Turtle.fp cacheDir)

                                return m

                        let reset Nothing = do
                                Turtle.unset "XDG_CACHE_HOME"
                            reset (Just x) = do
                                Turtle.export "XDG_CACHE_HOME" x

                        _ <- Turtle.managed (Exception.bracket set reset)

                        _ <- Turtle.liftIO load

                        return ()
                    else do
                        _ <- load

                        return ()

        let handler :: SomeException -> IO ()
            handler exception = Tasty.HUnit.assertFailure (show exception)

        Exception.handle handler runTest

        return () )

failureTest :: Text -> TestTree
failureTest path = do
    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString (do
        text <- Text.IO.readFile pathString

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        succeeded <- Exception.catch @SomeException
          (do _ <- Test.Util.load actualExpr
              return True
          )
          (\_ -> return False)

        if succeeded
            then fail "Import should have failed, but it succeeds"
            else return () )
