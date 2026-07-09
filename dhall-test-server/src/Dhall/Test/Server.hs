{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Server
    ( withServers
    ) where

import Control.Exception        (bracket, throwIO)
import Data.IORef               (IORef, atomicModifyIORef', newIORef)
import Network.HTTP.Types       (hContentType, hUserAgent, methodGet, status200, status404)
import Network.Wai              (Application, Request (..), responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setHost, setPort)
import Network.Wai.Handler.WarpTLS  (runTLS, tlsSettings)
import System.Directory         (doesFileExist)
import System.IO.Error          (mkIOError, userErrorType)

import qualified Control.Concurrent       as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import qualified Data.List                as List
import qualified Data.Maybe               as Maybe
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Network.Wai              as Wai
import qualified System.FilePath          as FilePath

httpPort :: Int
httpPort = 18080

httpsPort :: Int
httpsPort = 18443

certPath :: FilePath
certPath = "./dhall-test-server/cert/cert.pem"

keyPath :: FilePath
keyPath = "./dhall-test-server/cert/key.pem"

fallbackCertPath :: FilePath
fallbackCertPath = "../dhall-test-server/cert/cert.pem"

fallbackKeyPath :: FilePath
fallbackKeyPath = "../dhall-test-server/cert/key.pem"

withServers :: IO a -> IO a
withServers action = do
    (actualCertPath, actualKeyPath) <- resolveCertAndKeyPaths

    bracket (start actualCertPath actualKeyPath) stop (const action)
  where
    start actualCertPath actualKeyPath = do
        randomCounter <- newIORef 0
        httpReady <- Concurrent.newEmptyMVar
        httpsReady <- Concurrent.newEmptyMVar

        httpThread <- Async.async (runHttpServer httpReady (testHttpApp randomCounter))
        httpsThread <- Async.async (runHttpsServer actualCertPath actualKeyPath httpsReady (testHttpsApp randomCounter))

        Concurrent.takeMVar httpReady
        Concurrent.takeMVar httpsReady

        pure (httpThread, httpsThread)

    stop (httpThread, httpsThread) = do
        Async.cancel httpThread
        Async.cancel httpsThread

runHttpServer :: Concurrent.MVar () -> Application -> IO ()
runHttpServer ready app =
    runSettings
        (setBeforeMainLoop (Concurrent.putMVar ready ()) (setHost "127.0.0.1" (setPort httpPort defaultSettings)))
        app

runHttpsServer :: FilePath -> FilePath -> Concurrent.MVar () -> Application -> IO ()
runHttpsServer actualCertPath actualKeyPath ready app =
    runTLS
        (tlsSettings actualCertPath actualKeyPath)
        (setBeforeMainLoop (Concurrent.putMVar ready ()) (setHost "127.0.0.1" (setPort httpsPort defaultSettings)))
        app

resolveCertAndKeyPaths :: IO (FilePath, FilePath)
resolveCertAndKeyPaths = do
    primaryCertExists <- doesFileExist certPath
    primaryKeyExists <- doesFileExist keyPath

    if primaryCertExists && primaryKeyExists
        then pure (certPath, keyPath)
        else do
            fallbackCertExists <- doesFileExist fallbackCertPath
            fallbackKeyExists <- doesFileExist fallbackKeyPath

            if fallbackCertExists && fallbackKeyExists
                then pure (fallbackCertPath, fallbackKeyPath)
                else throwIO (mkIOError userErrorType "Missing TLS certificate files under dhall-test-server/cert" Nothing Nothing)

testHttpsApp :: IORef Int -> Application
testHttpsApp randomCounter request respond = do
    mResponse <- responseFromTestsFixtures request

    case mResponse of
        Just response -> respond response
        Nothing ->
            case (requestMethod request, pathInfo request) of
                (methodGet, ["user-agent"]) -> do
                    let userAgent = lookup hUserAgent (requestHeaders request)
                    respond (responseLBS status200 [(hContentType, "application/json")] (ByteString.Lazy.fromStrict (userAgentResponse userAgent)))

                (methodGet, ["random-string"]) -> do
                    n <- nextRandomCounter randomCounter
                    let body = "dhall-test-random-string-" <> BS8.pack (show n) <> "\n"
                    respond (responseLBS status200 [(hContentType, "text/plain")] (ByteString.Lazy.fromStrict body))

                (methodGet, ["foo"]) ->
                    if hasExampleTestHeader request then respond (dhallText "./bar") else respond response404

                (methodGet, ["bar"]) ->
                    if hasExampleTestHeader request then respond (dhallText "True") else respond response404

                (methodGet, ["cors", "AllowedAll.dhall"]) -> respond (corsText (Just "*") "42")
                (methodGet, ["cors", "OnlyOther.dhall"]) -> respond (corsText (Just "http://localhost:28080") "42")
                (methodGet, ["cors", "Empty.dhall"]) -> respond (corsText (Just "") "42")
                (methodGet, ["cors", "NoCORS.dhall"]) -> respond (corsText Nothing "42")
                (methodGet, ["cors", "Null.dhall"]) -> respond (corsText (Just "null") "42")
                (methodGet, ["cors", "SelfImportAbsolute.dhall"]) -> respond (corsText (Just "*") "http://127.0.0.1:18080/cors/NoCORS.dhall")
                (methodGet, ["cors", "SelfImportRelative.dhall"]) -> respond (corsText (Just "*") "./NoCORS.dhall")
                (methodGet, ["cors", "TwoHopsFail.dhall"]) -> respond (corsText (Just "*") "http://localhost:18080/tests/import/data/cors/OnlySelf.dhall")
                (methodGet, ["cors", "TwoHopsSuccess.dhall"]) -> respond (corsText (Just "*") "http://localhost:18080/tests/import/data/cors/OnlyGithub.dhall")

                _ -> respond response404

testHttpApp :: IORef Int -> Application
testHttpApp randomCounter request respond = do
    mResponse <- responseFromTestsFixtures request

    case mResponse of
        Just response -> respond response
        Nothing ->
            case (requestMethod request, pathInfo request) of
                (methodGet, ["user-agent"]) -> do
                    let userAgent = lookup hUserAgent (requestHeaders request)
                    respond (responseLBS status200 [(hContentType, "application/json")] (ByteString.Lazy.fromStrict (userAgentResponse userAgent)))

                (methodGet, ["random-string"]) -> do
                    n <- nextRandomCounter randomCounter
                    let body = "dhall-test-random-string-" <> BS8.pack (show n) <> "\n"
                    respond (responseLBS status200 [(hContentType, "text/plain")] (ByteString.Lazy.fromStrict body))

                (methodGet, ["foo", "..", "random-string"]) -> do
                    n <- nextRandomCounter randomCounter
                    let body = "dhall-test-random-string-" <> BS8.pack (show n) <> "\n"
                    respond (responseLBS status200 [(hContentType, "text/plain")] (ByteString.Lazy.fromStrict body))

                (methodGet, ["foo"]) ->
                    if hasExampleTestHeader request then respond (dhallText "./bar") else respond response404

                (methodGet, ["bar"]) ->
                    if hasExampleTestHeader request then respond (dhallText "True") else respond response404

                (methodGet, ["Prelude", "List", "length"]) ->
                    respond (corsText (Just "*") "List/length")

                (methodGet, ["nadrieril", "dhall", "tests", "import", "success", "unit", "asLocation", "Canonicalize3A.dhall"]) ->
                    respond (dhallText "./../bar/import.dhall as Location")

                (methodGet, ["nadrieril", "dhall", "tests", "import", "success", "unit", "asLocation", "Canonicalize5A.dhall"]) ->
                    respond (dhallText "./foo/../../bar/import.dhall as Location")

                (methodGet, ["nadrieril", "dhall", "tests", "import", "success", "unit", "asLocation", "MissingA.dhall"]) ->
                    respond (dhallText "missing as Location")

                (methodGet, ["nadrieril", "dhall", "tests", "import", "success", "unit", "asLocation", "EnvA.dhall"]) ->
                    respond (dhallText "env:HOME as Location")

                (methodGet, ["nadrieril", "dhall", "tests", "import", "success", "unit", "bar", "import.dhall"]) ->
                    respond (dhallText "2")

                (methodGet, ["cors", "AllowedAll.dhall"]) ->
                    respond (corsText (Just "*") "42")

                (methodGet, ["cors", "OnlySelf.dhall"]) ->
                    respond (corsText (Just "http://127.0.0.1:18080") "42")

                (methodGet, ["cors", "OnlyOther.dhall"]) ->
                    respond (corsText (Just "http://localhost:28080") "42")

                (methodGet, ["cors", "OnlyGithub.dhall"]) ->
                    respond (corsText (Just "http://localhost:18080") "42")

                (methodGet, ["cors", "Empty.dhall"]) ->
                    respond (corsText (Just "") "42")

                (methodGet, ["cors", "NoCORS.dhall"]) ->
                    respond (corsText Nothing "42")

                (methodGet, ["cors", "Null.dhall"]) ->
                    respond (corsText (Just "null") "42")

                (methodGet, ["cors", "SelfImportAbsolute.dhall"]) ->
                    respond (corsText (Just "*") "http://127.0.0.1:18080/cors/NoCORS.dhall")

                (methodGet, ["cors", "SelfImportRelative.dhall"]) ->
                    respond (corsText (Just "*") "./NoCORS.dhall")

                (methodGet, ["cors", "TwoHopsFail.dhall"]) ->
                    respond (corsText (Just "*") "http://localhost:18080/tests/import/data/cors/OnlySelf.dhall")

                (methodGet, ["cors", "TwoHopsSuccess.dhall"]) ->
                    respond (corsText (Just "*") "http://localhost:18080/tests/import/data/cors/OnlyGithub.dhall")

                _ -> respond response404

type TestsFixture = (FilePath, Bool) -- The Bool shows whether we need CORS information (which will be always "allow-origin *").

testsFixtures :: [TestsFixture]
testsFixtures =
    [ ("tests/import/data/example.txt", False)
    , ("tests/import/data/simple.dhall", False)
    , ("tests/import/data/cors/Prelude.dhall", False)
    , ("tests/import/data/simpleLocation.dhall", False)
    , ("tests/import/success/customHeadersA.dhall", False)
    , ("tests/import/data/referentiallyOpaque.dhall", False)
    , ("tests/import/data/cors/AllowedAll.dhall", True)
    , ("tests/import/data/cors/OnlyGithub.dhall", True)
    , ("tests/import/data/cors/OnlySelf.dhall", True)
    , ("tests/import/data/cors/OnlyOther.dhall", True)
    , ("tests/import/data/cors/Empty.dhall", True)
    , ("tests/import/data/cors/NoCORS.dhall", True)
    , ("tests/import/data/cors/Null.dhall", True)
    , ("tests/import/data/cors/SelfImportAbsolute.dhall", True)
    , ("tests/import/data/cors/SelfImportRelative.dhall", True)
    ]

responseFromTestsFixtures :: Request -> IO (Maybe Wai.Response)
responseFromTestsFixtures request
    | requestMethod request /= methodGet = pure Nothing
    | otherwise =
        case lookup (pathInfo request) testsFixtureRoutes of
            Nothing -> pure Nothing
            Just makeResponse -> Just <$> makeResponse

testsFixtureRoutes :: [([Text.Text], IO Wai.Response)]
testsFixtureRoutes = fmap toRoute testsFixtures
  where
    toRoute (shortPath, useCorsHeader) =
        let (urlSegments, _) = testsPathInfo shortPath
         in (urlSegments, testsFixtureResponse shortPath useCorsHeader)

testsPathInfo :: FilePath -> ([Text.Text], FilePath)
testsPathInfo shortPath =
    (Text.splitOn "/" (Text.pack shortPath), "dhall/dhall-lang" FilePath.</> shortPath)

testsFixtureResponse :: FilePath -> Bool -> IO Wai.Response
testsFixtureResponse shortPath useCorsHeader = do
    body <- readTestsFixtureFile shortPath
    pure
        (if useCorsHeader
            then corsText (Just "*") body
            else dhallText body
        )

readTestsFixtureFile :: FilePath -> IO BS8.ByteString
readTestsFixtureFile shortPath = do
    let (_, fullPath) = testsPathInfo shortPath
        noRepoPrefixPath = Maybe.fromMaybe fullPath (List.stripPrefix "dhall/" fullPath)
        candidates = List.nub [fullPath, noRepoPrefixPath, ".." FilePath.</> fullPath, ".." FilePath.</> noRepoPrefixPath]

    mResolved <- firstExistingPath candidates

    case mResolved of
        Just path -> BS8.readFile path
        Nothing -> throwIO (mkIOError userErrorType ("Missing test fixture file: " <> fullPath) Nothing Nothing)

firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (path : paths) = do
    exists <- doesFileExist path
    if exists
        then pure (Just path)
        else firstExistingPath paths

nextRandomCounter :: IORef Int -> IO Int
nextRandomCounter ref =
    atomicModifyIORef' ref (\n -> let n' = n + 1 in (n', n'))

userAgentResponse :: Maybe BS8.ByteString -> BS8.ByteString
userAgentResponse mUserAgent =
    "{\n  \"user-agent\": \"" <> agent <> "\"\n}\n"
  where
    agent = Maybe.fromMaybe "none_given" mUserAgent

hasExampleTestHeader :: Request -> Bool
hasExampleTestHeader request =
    fmap Text.toLower (decodeHeader =<< lookup "Test" (requestHeaders request)) == Just "example"
  where
    decodeHeader = either (const Nothing) Just . Text.decodeUtf8'

corsText :: Maybe BS8.ByteString -> BS8.ByteString -> Wai.Response
corsText mOrigin body =
    responseLBS status200 headers (ByteString.Lazy.fromStrict body)
  where
    baseHeaders = [(hContentType, "text/plain")]
    headers =
        case mOrigin of
            Nothing     -> baseHeaders
            Just origin -> ("Access-Control-Allow-Origin", origin) : baseHeaders

dhallText :: BS8.ByteString -> Wai.Response
dhallText = responseLBS status200 [(hContentType, "text/plain")] . ByteString.Lazy.fromStrict

response404 :: Wai.Response
response404 = responseLBS status404 [(hContentType, "text/plain")] "Not Found\n"
