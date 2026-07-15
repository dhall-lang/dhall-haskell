{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Dhall.Test.Import where

import Control.Exception (SomeException)
import Data.Text         (Text)
import Data.Void         (Void)
#if __GLASGOW_HASKELL__ >= 908
import Data.Default      (def)
#endif
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
import qualified System.Directory                 as Directory
import qualified System.FilePath                  as FilePath
import qualified System.IO.Temp                   as Temp
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.HUnit                 as Tasty.HUnit
import qualified Turtle

#if defined(WITH_HTTP) && defined(NETWORK_TESTS)
import qualified Network.Connection      as Connection
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
#if __GLASGOW_HASKELL__ >= 908
import Network.TLS             (Supported(..))
#endif
#endif


importDirectory :: FilePath
importDirectory = "./dhall-lang/tests/import"

getTests :: IO TestTree
getTests = do
    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest $ (do
        path <- Turtle.lstree (importDirectory </> "success")

        return path )

    failureTests <- Test.Util.discover (Turtle.chars <* ".dhall") failureTest (do
        path <- Turtle.lstree (importDirectory </> "failure")

        let expectedSuccesses =
                [ importDirectory </> "failure/unit/DontRecoverCycle.dhall"
                , importDirectory </> "failure/unit/DontRecoverTypeError.dhall"
#if !(defined(WITH_HTTP) && defined(NETWORK_TESTS))
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

    let expectedFailures = []

    Test.Util.testCase prefix expectedFailures (do

        text <- Text.IO.readFile inputPath

        expectedText <- Text.IO.readFile expectedPath

        actualExpr <- Core.throws (Parser.exprFromText mempty text)

        expectedExpr <- Core.throws (Parser.exprFromText mempty expectedText)

        homeDirectory <- Directory.makeAbsolute (importDirectory </> "home")

        let originalCache = "dhall-lang/tests/import/cache"

#if defined(WITH_HTTP) && defined(NETWORK_TESTS)
        let testTlsSettings =
#if __GLASGOW_HASKELL__ >= 906
-- note: MIN_VERSION_crypton_connection is defined only if we are building with GHC 9.6 and later
#if MIN_VERSION_crypton_connection(0,4,0)
                let defaultSupported :: Supported
                    defaultSupported = def
                in Connection.TLSSettingsSimple
                    { Connection.settingDisableCertificateValidation = True
                    , Connection.settingDisableSession = False
                    , Connection.settingUseServerName = True
                    , Connection.settingClientSupported = defaultSupported
                    }
#else
                Connection.TLSSettingsSimple
                    { Connection.settingDisableCertificateValidation = True
                    , Connection.settingDisableSession = False
                    , Connection.settingUseServerName = True
                    }
#endif
#else
                Connection.TLSSettingsSimple
                    { Connection.settingDisableCertificateValidation = True
                    , Connection.settingDisableSession = False
                    , Connection.settingUseServerName = True
                    }
#endif

        let httpManager =
                HTTP.newManager
                    (HTTP.mkManagerSettings testTlsSettings Nothing)
                        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (120 * 1000 * 1000) }

        let status =
                Import.makeEmptyStatus
                    httpManager
                    (pure Import.envOriginHeaders)
                    directoryString
#else
        let status = Import.emptyStatus directoryString
#endif

        let status' =
                status
                    { Import._reportWarning = \_ -> return ()
                    , Import._getHomeDirectory = pure homeDirectory
                    }

        let load =
                State.evalStateT
                    (Test.Util.loadWith actualExpr)
                    status'

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

        homeDirectory <- Directory.makeAbsolute (importDirectory </> "home")

        let status =
                (Import.emptyStatus ".")
                    { Import._getHomeDirectory = pure homeDirectory }

        let setup = Test.Util.managedTestEnvironment prefix

        let run = Exception.catch @SomeException
              (State.evalStateT (Test.Util.loadWith actualExpr) status >> return True)
              (\_ -> return False)

        succeeded <- Turtle.with setup (const run)

        if succeeded
            then fail "Import should have failed, but it succeeds"
            else return () )
