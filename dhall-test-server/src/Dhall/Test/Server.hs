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
        -- Both http and https APIs are identical, for simplicity.
        httpReady <- Concurrent.newEmptyMVar
        httpsReady <- Concurrent.newEmptyMVar

        httpThread <- Async.async (runHttpServer httpReady (testHttpApp randomCounter))
        httpsThread <- Async.async (runHttpsServer actualCertPath actualKeyPath httpsReady (testHttpApp randomCounter))

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

testHttpApp :: IORef Int -> Application
testHttpApp randomCounter request respond = do
    mResponse <- responseFromTestFixtures request

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
                    respond (corsText (Just "https://127.0.0.1:18080") "42")

                (methodGet, ["cors", "OnlyOther.dhall"]) ->
                    respond (corsText (Just "https://localhost:28080") "42")

                (methodGet, ["cors", "OnlyGithub.dhall"]) ->
                    respond (corsText (Just "https://localhost:18443") "42")

                (methodGet, ["cors", "Empty.dhall"]) ->
                    respond (corsText (Just "") "42")

                (methodGet, ["cors", "NoCORS.dhall"]) ->
                    respond (corsText Nothing "42")

                (methodGet, ["cors", "Null.dhall"]) ->
                    respond (corsText (Just "null") "42")

                (methodGet, ["cors", "SelfImportAbsolute.dhall"]) ->
                    respond (corsText (Just "*") "https://127.0.0.1:18080/cors/NoCORS.dhall")

                (methodGet, ["cors", "SelfImportRelative.dhall"]) ->
                    respond (corsText (Just "*") "./NoCORS.dhall")

                (methodGet, ["cors", "TwoHopsFail.dhall"]) ->
                    respond (corsText (Just "*") "https://localhost:18443/tests/import/data/cors/OnlySelf.dhall")

                (methodGet, ["cors", "TwoHopsSuccess.dhall"]) ->
                    respond (corsText (Just "*") "https://localhost:18443/tests/import/data/cors/OnlyGithub.dhall")

                _ -> respond response404

-- The server responds to GET /tests/import/... requests by serving the
-- corresponding file from dhall/dhall-lang/tests/import/... . Files located in a
-- `cors` directory are served with an `Access-Control-Allow-Origin: *` header.
responseFromTestFixtures :: Request -> IO (Maybe Wai.Response)
responseFromTestFixtures request
    | requestMethod request /= methodGet = pure Nothing
    | otherwise =
        case pathInfo request of
            ("tests" : "import" : rest) -> do
                let shortPath = "tests" FilePath.</> "import" FilePath.</> FilePath.joinPath (fmap Text.unpack rest)
                mResponse <- serveTestFixture shortPath
                pure (Just (Maybe.fromMaybe response404 mResponse))
            _ -> pure Nothing

serveTestFixture :: FilePath -> IO (Maybe Wai.Response)
serveTestFixture shortPath = do
    mFixturePath <- resolveTestFixturePath shortPath
    case mFixturePath of
        Nothing -> pure Nothing
        Just path -> do
            body <- normalizeWindowsLineEndings <$> BS8.readFile path
            let response = if (isCorsFixture shortPath) then corsText (Just "*") body else dhallText body
            pure $ Just response
 where
  resolveTestFixturePath :: FilePath -> IO (Maybe FilePath)
  resolveTestFixturePath shortPath =
    let fullPath = "dhall/dhall-lang" FilePath.</> shortPath
        noRepoPrefixPath = Maybe.fromMaybe fullPath (List.stripPrefix "dhall/" fullPath)
        candidates = List.nub [fullPath, noRepoPrefixPath, ".." FilePath.</> fullPath, ".." FilePath.</> noRepoPrefixPath]
     in firstExistingPath candidates

  isCorsFixture :: FilePath -> Bool
  isCorsFixture shortPath = "cors" `elem` FilePath.splitDirectories shortPath

  -- Always return Unix line endings, even under Windows.
  normalizeWindowsLineEndings :: BS8.ByteString -> BS8.ByteString
  normalizeWindowsLineEndings = BS8.intercalate "\n" . fmap stripTrailingCarriageReturn . BS8.split '\n'

  stripTrailingCarriageReturn line
        | not (BS8.null line) && BS8.last line == '\r' = BS8.init line
        | otherwise = line

  firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
  firstExistingPath [] = pure Nothing
  firstExistingPath (path : paths) = do
    exists <- doesFileExist path
    if exists then pure (Just path) else firstExistingPath paths

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
