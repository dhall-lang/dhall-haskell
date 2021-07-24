{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Dhall.Test.Import where

import Control.Exception (SomeException)
import Data.Text         (Text)
import Data.Void         (Void)
import Prelude           hiding (FilePath)
import Test.Tasty        (TestTree)
import Turtle            (FilePath, (</>))
import Dhall.Import.Manager (Manager(..))

import qualified Control.Exception                as Exception
import qualified Control.Monad                    as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Import.UserHeaders as UserHeaders
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified Network.HTTP.Client              as HTTP
import qualified Network.HTTP.Client.TLS          as HTTP
import qualified System.FilePath                  as FilePath
import qualified System.IO.Temp                   as Temp
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

    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest (do
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
successTest prefix = do
    let inputPath = Text.unpack (prefix <> "A.dhall")

    let expectedPath = Text.unpack (prefix <> "B.dhall")

    let directoryString = FilePath.takeDirectory inputPath

    let expectedFailures =
            [ importDirectory </> "success/unit/cors/TwoHops"
            , importDirectory </> "success/unit/cors/SelfImportAbsolute"
            , importDirectory </> "success/unit/cors/AllowedAll"
            , importDirectory </> "success/unit/cors/SelfImportRelative"
            , importDirectory </> "success/unit/cors/OnlyGithub"
            ]

    Test.Util.testCase prefix expectedFailures (do

        text <- Text.IO.readFile inputPath

        expectedText <- Text.IO.readFile expectedPath

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        expectedExpr <- Core.throws (Parser.exprFromText mempty expectedText)

        let originalCache = "dhall-lang/tests/import/cache"

        let newHttpManager =
                HTTP.newManager
                    HTTP.tlsManagerSettings
                        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (120 * 1000 * 1000) }

        let newManager = do
                http <- newHttpManager
                return Manager {
                    httpManager = http,
                    headersManager = UserHeaders.noopUserHeaders
                }

        let load =
                State.evalStateT
                    (Test.Util.loadWith actualExpr)
                    (Import.emptyStatusWithManager newManager directoryString)

        let usesCache = [ "hashFromCache"
                        , "unit/asLocation/Hash"
                        , "unit/IgnorePoisonedCache"
                        , "unit/DontCacheIfHash"
                        ]

        let endsIn path' =
                not (null (Turtle.match (Turtle.ends path') (Test.Util.toDhallPath prefix)))

        let buildNewCache = do
                tempdir <- fmap Turtle.decodeString (Turtle.managed (Temp.withSystemTempDirectory "dhall-cache"))
                Turtle.liftIO (Turtle.cptree originalCache tempdir)
                return tempdir

        let cacheSetup =
                if any endsIn usesCache
                    then do
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
                        return ()
                else pure ()

        let setup = cacheSetup >> Test.Util.managedTestEnvironment prefix

        let resolve = Turtle.with setup (const load)

        let handler :: SomeException -> IO (Core.Expr Parser.Src Void)
            handler exception = Tasty.HUnit.assertFailure (show exception)

        actualResolved <- Exception.handle handler resolve

        expectedResolved <- Import.assertNoImports expectedExpr

        let actual = Core.normalize actualResolved :: Core.Expr Void Void

        let expected = Core.normalize expectedResolved :: Core.Expr Void Void

        let message =
                "The imported expression did not match the expected output"

        Tasty.HUnit.assertEqual message expected actual)

failureTest :: Text -> TestTree
failureTest path = do
    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString (do
        actualExpr <- do
          Core.throws (Parser.exprFromText mempty (Test.Util.toDhallPath path))

        succeeded <- Exception.catch @SomeException
          (do _ <- Test.Util.load actualExpr
              return True
          )
          (\_ -> return False)

        if succeeded
            then fail "Import should have failed, but it succeeds"
            else return () )
