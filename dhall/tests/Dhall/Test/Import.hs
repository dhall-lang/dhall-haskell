{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Dhall.Test.Import where

import Control.Exception (SomeException)
import Data.Text         (Text)
import Data.Void         (Void)
import System.FilePath   ((</>))
import Test.Tasty        (TestTree)

import qualified Control.Exception                as Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified System.FilePath                  as FilePath
import qualified System.IO.Temp                   as Temp
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.HUnit                 as Tasty.HUnit
import qualified Turtle

#if defined(WITH_HTTP) && defined(NETWORK_TESTS)
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
#endif


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

              -- Skip all tests that reference httpbin.org to avoid clobbering
              -- their servers.  These should eventually be replaced by tests
              -- that depend on an equivalent endpoint on test.dhall-lang.org
              -- instead of httpbin.org.
            , importDirectory </> "failure/customHeadersUsingBoundVariable.dhall"
            , importDirectory </> "failure/originHeadersFromRemote.dhall"
            , importDirectory </> "failure/originHeadersFromRemoteENV.dhall"
            , importDirectory </> "success/customHeadersA.dhall"
            , importDirectory </> "success/noHeaderForwardingA.dhall"
            , importDirectory </> "success/success/originHeadersA.dhall"
            , importDirectory </> "success/originHeadersENV.dhall"
            , importDirectory </> "success/originHeadersImportA.dhall"
            , importDirectory </> "success/originHeadersImportENV.dhall"
            , importDirectory </> "success/originHeadersImportFromEnvA.dhall"
            , importDirectory </> "success/originHeadersImportFromEnvENV.dhall"
            , importDirectory </> "success/originHeadersOverrideA.dhall"
            , importDirectory </> "success/originHeadersOverrideENV.dhall"
            ]

    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest (do
        path <- Turtle.lstree (importDirectory </> "success")

        path `Test.Util.pathNotIn` flakyTests

        return path )

    failureTests <- Test.Util.discover (Turtle.chars <* ".dhall") failureTest (do
        path <- Turtle.lstree (importDirectory </> "failure")

        let expectedSuccesses =
                [ importDirectory </> "failure/unit/DontRecoverCycle.dhall"
                , importDirectory </> "failure/unit/DontRecoverTypeError.dhall"
#if !(defined(WITH_HTTP) && defined(NETWORK_TESTS))
                -- We attempt to simulate test.dhall-lang.org, but even so
                -- some tests unexpectedly succeed due to the inadequacy of
                -- the simulation
                , importDirectory </> "failure/unit/cors/OnlySelf.dhall"
                , importDirectory </> "failure/unit/cors/OnlyOther.dhall"
                , importDirectory </> "failure/unit/cors/Null.dhall"
                , importDirectory </> "failure/unit/cors/TwoHops.dhall"
                , importDirectory </> "failure/unit/cors/Empty.dhall"
                , importDirectory </> "failure/unit/cors/NoCORS.dhall"
                , importDirectory </> "failure/originHeadersFromRemote.dhall"
#endif
                ]

        path `Test.Util.pathNotIn` expectedSuccesses
        "ENV.dhall" `Test.Util.pathNotSuffixOf` path

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
            [
              -- Importing relative to the home directory works, but I'm too
              -- lazy to mock the home directory for testing purposes
              importDirectory </> "success/unit/ImportRelativeToHome"
#if !(defined(WITH_HTTP) && defined(NETWORK_TESTS))
            , importDirectory </> "success/originHeadersImportFromEnv"
            , importDirectory </> "success/originHeadersImport"
            , importDirectory </> "success/originHeadersOverride"
            , importDirectory </> "success/unit/asLocation/RemoteChainEnv"
#endif
            ]

    Test.Util.testCase prefix expectedFailures (do

        text <- Text.IO.readFile inputPath

        expectedText <- Text.IO.readFile expectedPath

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        expectedExpr <- Core.throws (Parser.exprFromText mempty expectedText)

        let originalCache = "dhall-lang/tests/import/cache"

#if defined(WITH_HTTP) && defined(NETWORK_TESTS)
        let httpManager =
                HTTP.newManager
                    HTTP.tlsManagerSettings
                        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (120 * 1000 * 1000) }

        let status =
                Import.makeEmptyStatus
                    httpManager
                    (pure Import.envOriginHeaders)
                    directoryString
#else
        let status = Import.emptyStatus directoryString
#endif

        let load =
                State.evalStateT
                    (Test.Util.loadWith actualExpr)
                    status

        let usesCache = [ "hashFromCache"
                        , "unit/asLocation/Hash"
                        , "unit/IgnorePoisonedCache"
                        , "unit/DontCacheIfHash"
                        ]

        let endsIn path' =
                not (null (Turtle.match (Turtle.ends path') (Test.Util.toDhallPath prefix)))

        let buildNewCache = do
                tempdir <- Turtle.managed (Temp.withSystemTempDirectory "dhall-cache")
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
failureTest prefix = do
    let path = prefix <> ".dhall"

    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString (do
        actualExpr <- do
          Core.throws (Parser.exprFromText mempty (Test.Util.toDhallPath path))

        let setup = Test.Util.managedTestEnvironment prefix

        let run = Exception.catch @SomeException
              (Test.Util.load actualExpr >> return True)
              (\_ -> return False)

        succeeded <- Turtle.with setup (const run)

        if succeeded
            then fail "Import should have failed, but it succeeds"
            else return () )
