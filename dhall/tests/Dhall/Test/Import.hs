{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Dhall.Test.Import where

import Control.Exception (Exception, SomeException)
import Data.Text         (Text)
import Data.Void         (Void)
#if __GLASGOW_HASKELL__ >= 906
import Data.Default      (def)
#endif
import System.FilePath   ((</>))
import Test.Tasty        (TestTree)

import qualified Control.Exception                as Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString.Char8            as ByteString.Char8
import qualified Data.List                        as List
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Crypto
import qualified Dhall.Map                        as Map
import qualified Dhall
import qualified Lens.Micro                       as Lens
import qualified Dhall.Import                     as Import
import qualified Dhall.Parser                     as Parser
import qualified Dhall.Test.Util                  as Test.Util
import qualified System.Directory                 as Directory
import qualified System.FilePath                  as FilePath
import qualified System.IO.Temp                   as Temp
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.HUnit                 as Tasty.HUnit
import qualified Turtle

#if defined(WITH_HTTP)
import qualified Network.Connection      as Connection
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
#if __GLASGOW_HASKELL__ >= 906
import Network.TLS             (Supported(..))
#endif
#endif


importDirectory :: FilePath
importDirectory = "./dhall-lang/tests/import"

getTests :: IO TestTree
getTests = do
    successTests <- Test.Util.discover (Turtle.chars <* "A.dhall") successTest (do
        path <- Turtle.lstree (importDirectory </> "success")

#if !defined(WITH_HTTP)
        -- HTTP is compiled out (`-f-with-http`); skip tests that need the
        -- local test server.
        "cors" `Test.Util.pathNotInfixOf` path
        "Remote" `Test.Util.pathNotInfixOf` path
        "header" `Test.Util.pathNotInfixOf` path
        "Header" `Test.Util.pathNotInfixOf` path
        "originHeaders" `Test.Util.pathNotInfixOf` path
        "customHeaders" `Test.Util.pathNotInfixOf` path
        "normalCachingOfProtected" `Test.Util.pathNotInfixOf` path
#endif

        return path )

    failureTests <- Test.Util.discover (Turtle.chars <* ".dhall") failureTest (do
        path <- Turtle.lstree (importDirectory </> "failure")

        let expectedSuccesses =
                [ importDirectory </> "failure/unit/DontRecoverCycle.dhall"
                , importDirectory </> "failure/unit/DontRecoverTypeError.dhall"
#if !defined(WITH_HTTP)
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
                , plainImportErrorTests
                , collectImportErrorTests
                , sharedImportEvaluationTests
                , customNormalizerEvaluatesInlinedTree
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

        let status = importStatus directoryString

        let status' =
                status
                    { Import._reportWarning = \_ -> return ()
                    , Import._getHomeDirectory = pure homeDirectory
                    }

        let load =
                State.evalStateT
                    (importLoadWith actualExpr)
                    status'

        let usesCache = [ "hashFromCache"
                        , "unit/asLocation/Hash"
                        , "unit/IgnorePoisonedCache"
                        , "unit/DontCacheIfHash"
                        , "normalCachingOfProtected"
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
                (importStatus ".")
                    { Import._getHomeDirectory = pure homeDirectory }

        let setup = Test.Util.managedTestEnvironment prefix

        let run = Exception.catch @SomeException
              (State.evalStateT (importLoadWith actualExpr) status >> return True)
              (\_ -> return False)

        succeeded <- Turtle.with setup (const run)

        if succeeded
            then fail "Import should have failed, but it succeeds"
            else return () )

-- | Use the real importer against `dhall-test-server` (localhost) whenever HTTP
-- is compiled in.  `Test.Util.loadWith` still mocks remotes when
-- `-f-network-tests` is set, which is only needed for non-import tests and for
-- Tutorial doctests that hit the public internet.
#if defined(WITH_HTTP)
importLoadWith :: Core.Expr Parser.Src Core.Import -> State.StateT Import.Status IO (Core.Expr Parser.Src Void)
importLoadWith = Import.loadWith

importStatus :: FilePath -> Import.Status
importStatus directoryString =
    Import.makeEmptyStatus
        testHttpManager
        (pure Import.envOriginHeaders)
        directoryString

testHttpManager :: IO Import.Manager
testHttpManager =
    HTTP.newManager
        (HTTP.mkManagerSettings testTlsSettings Nothing)
            { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (120 * 1000 * 1000) }

testTlsSettings :: Connection.TLSSettings
testTlsSettings =
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
#else
importLoadWith :: Core.Expr Parser.Src Core.Import -> State.StateT Import.Status IO (Core.Expr Parser.Src Void)
importLoadWith = Test.Util.loadWith

importStatus :: FilePath -> Import.Status
importStatus = Import.emptyStatus
#endif

-- | A fold imported through two files is evaluated once, and the normal form
--   matches normalizing the fully inlined tree.  @site1@ projects away the
--   expensive field.
sharedImportEvaluationTests :: TestTree
sharedImportEvaluationTests =
    Tasty.HUnit.testCase "shared import evaluation matches inlined normalization" $
        Temp.withSystemTempDirectory "dhall-shared-import" $ \dir -> do
            let write name text = Text.IO.writeFile (dir </> name) text
            write "prelude.dhall" $ Text.unlines
                [ "let increment = \\(x : Natural) -> x + 1"
                , "let factor = 4"
                , "let expensive = Natural/fold factor Natural increment 0"
                , "let cheap = 1"
                , "in { increment, expensive, cheap }"
                ]
            write "site1.dhall"
                "let Prelude = ./prelude.dhall in Prelude.increment Prelude.cheap\n"
            write "site2.dhall"
                "let Prelude = ./prelude.dhall in Prelude.expensive\n"
            write "main.dhall"
                "{ used = ./site2.dhall, ignored = ./site1.dhall }\n"

            (site1Shared, site1Inlined) <- normalizePair dir "site1.dhall"
            site1Shared Tasty.HUnit.@?= site1Inlined
            site1Shared Tasty.HUnit.@?= Core.NaturalLit 2

            (mainShared, mainInlined) <- normalizePair dir "main.dhall"
            mainShared Tasty.HUnit.@?= mainInlined

-- | A custom normalizer is applied to the fully inlined tree.  Shared
--   evaluation does not consult it, so this result is the rewritten natural
--   at every use site.
customNormalizerEvaluatesInlinedTree :: TestTree
customNormalizerEvaluatesInlinedTree =
    Tasty.HUnit.testCase "custom normalizer evaluates the inlined tree" $
        Temp.withSystemTempDirectory "dhall-custom-normalizer" $ \dir -> do
            Text.IO.writeFile (dir </> "shared.dhall") "0\n"
            let normalizer (Core.NaturalLit 0) = Just (Core.NaturalLit 7)
                normalizer _ = Nothing
                settings =
                    Lens.set
                        Dhall.normalizer
                        (Just (Core.ReifiedNormalizer (pure . normalizer)))
                        (Lens.set Dhall.rootDirectory dir Dhall.defaultInputSettings)
            result <-
                Dhall.inputExprWithSettings
                    settings
                    "{ a = ./shared.dhall, b = ./shared.dhall }\n"
            let expected =
                    Core.RecordLit
                        ( Map.fromList
                            [ ("a", Core.makeRecordField (Core.NaturalLit 7))
                            , ("b", Core.makeRecordField (Core.NaturalLit 7))
                            ]
                        )
            Core.denote result Tasty.HUnit.@?= (expected :: Core.Expr Void Void)

normalizePair
    :: FilePath
    -> FilePath
    -> IO (Core.Expr Void Void, Core.Expr Void Void)
normalizePair dir file = do
    text <- Text.IO.readFile (dir </> file)
    parsed <- Core.throws (Parser.exprFromText file text)
    let status =
            (Import.emptyStatus dir)
                { Import._semanticCacheMode = Import.IgnoreSemanticCache }
    ((inlined, twin), status') <-
        State.runStateT (Import.loadWithShared parsed) status
    let sharedNF = Core.denote (Import.normalizeLoaded status' twin)
        inlinedNF = Core.denote (Core.normalize inlined)
    return (sharedNF, inlinedNF)

-- | 'Show' for import errors embeds ANSI colour.  'Import.plainShowImportError'
--   is that same text with the colour codes removed.
plainImportErrorTests :: TestTree
plainImportErrorTests =
    Tasty.testGroup "plain import errors"
        [ assertPlainImportError
            "missing file"
            (Import.MissingImports
                [Exception.toException (Import.MissingFile "missing.dhall")]
            )
            ["Missing file", "missing.dhall"]
        , assertPlainImportError
            "missing environment variable"
            (Import.MissingEnvironmentVariable "DHALL_MISSING")
            ["Missing environment variable", "DHALL_MISSING"]
        , assertPlainImportError
            "no valid imports"
            (Import.MissingImports [])
            ["No valid imports"]
        , assertPlainImportError
            "several failed imports"
            (Import.MissingImports
                [ Exception.toException (Import.MissingFile "a.dhall")
                , Exception.toException
                    (Import.MissingEnvironmentVariable "NOT_SET")
                ]
            )
            [ "Failed to resolve imports"
            , "Missing file"
            , "a.dhall"
            , "Missing environment variable"
            , "NOT_SET"
            ]
        , assertPlainImportError
            "hash mismatch"
            (Import.HashMismatch
                { Import.expectedHash =
                    Dhall.Crypto.sha256Hash (ByteString.Char8.pack "expected")
                , Import.actualHash =
                    Dhall.Crypto.sha256Hash (ByteString.Char8.pack "actual")
                }
            )
            ["Import integrity check failed", "Expected hash:", "Actual hash:"]
        ]

assertPlainImportError :: Exception e => String -> e -> [String] -> TestTree
assertPlainImportError name exception fragments =
    Tasty.HUnit.testCase name $ do
        let coloured = show exception
        Tasty.HUnit.assertBool
            ("Show output should contain ANSI colour codes:\n" ++ coloured)
            (hasAnsi coloured)
        let plain = Import.plainShowImportError (Exception.toException exception)
        Tasty.HUnit.assertBool
            ("plainShowImportError should not contain ANSI colour codes:\n" ++ plain)
            (not (hasAnsi plain))
        mapM_
            (\fragment -> do
                Tasty.HUnit.assertBool
                    ("Show output should contain " ++ show fragment)
                    (fragment `List.isInfixOf` coloured)
                Tasty.HUnit.assertBool
                    ("plain output should contain " ++ show fragment)
                    (fragment `List.isInfixOf` plain)
            )
            fragments

hasAnsi :: String -> Bool
hasAnsi text = "\ESC[" `List.isInfixOf` text

-- | 'loadRelativeTo' throws on the first missing import.  'loadCollecting'
--   records every one.
collectImportErrorTests :: TestTree
collectImportErrorTests =
    Tasty.testGroup "collected import errors"
        [ Tasty.HUnit.testCase "load stops at the first missing import" $
            withTwoMissingImports $ \dir -> do
                expr <- parseParent dir
                result <-
                    Exception.try @SomeException
                        (Import.loadRelativeTo dir Import.IgnoreSemanticCache expr)
                case result of
                    Right _ ->
                        Tasty.HUnit.assertFailure "load succeeded"
                    Left err -> do
                        let paths = missingFilePaths err
                        assertPathMentioned "missing-a.dhall" paths
                        assertPathAbsent "missing-b.dhall" paths
        , Tasty.HUnit.testCase "CollectErrors returns every missing import" $
            withTwoMissingImports $ \dir -> do
                expr <- parseParent dir
                (_resolved, errs) <-
                    Import.loadCollecting dir Import.IgnoreSemanticCache expr
                let paths =
                        concatMap
                            (concatMap missingFilePaths . Import.collectedErrors)
                            errs
                Tasty.HUnit.assertEqual "number of failures" 2 (length errs)
                assertPathMentioned "missing-a.dhall" paths
                assertPathMentioned "missing-b.dhall" paths
        ]

withTwoMissingImports :: (FilePath -> IO a) -> IO a
withTwoMissingImports action =
    Temp.withSystemTempDirectory "dhall-collect-imports" $ \dir -> do
        Text.IO.writeFile
            (dir </> "parent.dhall")
            "[ ./missing-a.dhall, ./missing-b.dhall ]\n"
        action dir

parseParent :: FilePath -> IO (Core.Expr Parser.Src Core.Import)
parseParent dir = do
    text <- Text.IO.readFile (dir </> "parent.dhall")
    Core.throws (Parser.exprFromText mempty text)

missingFilePaths :: SomeException -> [FilePath]
missingFilePaths exception =
    case Exception.fromException @(Parser.SourcedException Import.MissingImports) exception of
        Just (Parser.SourcedException _ (Import.MissingImports es)) ->
            concatMap missingFilePaths es
        Nothing ->
            case Exception.fromException @Import.MissingImports exception of
                Just (Import.MissingImports es) ->
                    concatMap missingFilePaths es
                Nothing ->
                    case Exception.fromException @(Import.Imported Import.MissingFile) exception of
                        Just (Import.Imported _ (Import.MissingFile path)) ->
                            [path]
                        Nothing ->
                            case Exception.fromException @Import.MissingFile exception of
                                Just (Import.MissingFile path) ->
                                    [path]
                                Nothing ->
                                    []

assertPathMentioned :: String -> [FilePath] -> IO ()
assertPathMentioned fragment paths =
    Tasty.HUnit.assertBool
        (fragment ++ " missing from " ++ show paths)
        (any (List.isInfixOf fragment) paths)

assertPathAbsent :: String -> [FilePath] -> IO ()
assertPathAbsent fragment paths =
    Tasty.HUnit.assertBool
        (fragment ++ " unexpectedly in " ++ show paths)
        (all (not . List.isInfixOf fragment) paths)
