{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Import.HTTP
    ( fetchFromHttpUrl
    , fetchFromHttpUrlBytes
    , originHeadersFileExpr
    ) where

import Control.Exception                (Exception)
import Control.Monad.IO.Class           (MonadIO (..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString                  (ByteString)
import Data.CaseInsensitive             (CI)
import Data.Dynamic                     (toDyn)
import Data.List.NonEmpty               (NonEmpty (..))
import Data.Text.Encoding               (decodeUtf8)
import Dhall.Core
    ( Expr (..)
    , Directory (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportMode (..)
    , ImportType (..)
    , Scheme (..)
    , URL (..)
    )
import Dhall.Import.Types
import Dhall.Parser                     (Src)
import Dhall.URL                        (renderURL)
import System.Directory                 (getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath                  (splitDirectories)


import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString.Lazy             as ByteString.Lazy
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding
import qualified Dhall.Util
import qualified Network.HTTP.Client              as HTTP
import qualified Network.HTTP.Types

mkPrettyHttpException :: String -> HttpException -> PrettyHttpException
mkPrettyHttpException url ex =
    PrettyHttpException (renderPrettyHttpException url ex) (toDyn ex)

renderPrettyHttpException :: String -> HttpException -> String
renderPrettyHttpException _ (InvalidUrlException _ r) =
      "\n"
  <>  "\ESC[1;31mError\ESC[0m: Invalid URL\n"
  <>  "\n"
  <>  "URL: " <> show r <> "\n"
renderPrettyHttpException url (HttpExceptionRequest _ e) =
  case e of
    ConnectionFailure _ ->
          "\n"
      <>  "\ESC[1;31mError\ESC[0m: Remote host not found\n"
      <>  "\n"
      <>  "URL: " <> url <> "\n"
    InvalidDestinationHost host ->
          "\n"
      <>  "\ESC[1;31mError\ESC[0m: Invalid remote host name:\n"
      <>  "\n"
      <>  "Host: " <> show host <> "\n"
    ResponseTimeout ->
          "\n"
      <>  "\ESC[1;31mError\ESC[0m: The remote host took too long to respond\n"
      <>  "\n"
      <>  "URL: " <> url <> "\n"
    ConnectionTimeout ->
          "\n"
      <>  "\ESC[1;31mError\ESC[0m: Connection establishment took too long\n"
      <>  "\n"
      <>  "URL: " <> url <> "\n"
    StatusCodeException response body -> prefix <> suffix
      where
        prefix
            | statusCode == 401 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Access unauthorized\n"
            | statusCode == 403 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Access forbidden\n"
            | statusCode == 404 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Remote file not found\n"
            | statusCode == 500 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Server-side failure\n"
            | statusCode == 502 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Upstream failure\n"
            | statusCode == 503 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Server temporarily unavailable\n"
            | statusCode == 504 =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: Upstream timeout\n"
            | otherwise =
                    "\n"
                <>  "\ESC[1;31mError\ESC[0m: HTTP request failure\n"

        suffix =
                "\n"
            <>  "HTTP status code: " <> show statusCode <> "\n"
            <>  "\n"
            <>  "URL: " <> url <> "\n"
            <>  message

        statusCode =
            Network.HTTP.Types.statusCode
                (HTTP.responseStatus response)

        message =
            case Data.Text.Encoding.decodeUtf8' body of
                Left _ ->
                        "\n"
                    <>  "Message (non-UTF8 bytes):\n"
                    <>  "\n"
                    <>  truncatedBodyString <> "\n"
                  where
                    bodyString = show body

                    dots = "…"

                    truncatedLength = 80 - length dots

                    truncatedBodyString
                        | truncatedLength < length bodyString =
                            take truncatedLength bodyString <> dots
                        | otherwise =
                            bodyString
                Right "" ->
                    ""
                Right bodyText ->
                        "\n"
                    <>  "Message:\n"
                    <>  "\n"
                    <>  Text.unpack prefixedText
                  where
                    prefixedLines =
                        zipWith combine prefixes (Text.lines bodyText)
                      where
                        prefixes =
                            map (Text.pack . show) [(1 ::Int)..7] ++ [ "…" ]

                        combine n line = n <> "│ " <> line

                    prefixedText = Text.unlines prefixedLines

    e' -> "\n"
      <>  "\ESC[1;31mError\ESC[0m: " <> show e'
      <>  "\n"
      <>  "URL: " <> url <> "\n"

newManager :: StateT Status IO Manager
newManager = do
    Status { _manager = oldManager, ..} <- State.get

    case oldManager of
        Nothing -> do
            manager <- liftIO _newManager

            State.put (Status { _manager = Just manager , ..})

            return manager

        Just manager ->
            return manager

data NotCORSCompliant = NotCORSCompliant
    { expectedOrigins :: [ByteString]
    , actualOrigin    :: ByteString
    , childURL        :: URL
    , parentURL       :: URL
    }

instance Exception NotCORSCompliant

instance Show NotCORSCompliant where
    show (NotCORSCompliant {..}) =
            Dhall.Util._ERROR <> ": Not CORS compliant\n"
        <>  "\n"
        <>  "Dhall supports transitive imports, meaning that an imported expression can\n"
        <>  "import other expressions.  However, a remote import (the \"parent\" import)\n"
        <>  "cannot import another remote import (the \"child\" import) unless the child\n"
        <>  "import grants permission to do using CORS.  The child import must respond with\n"
        <>  "an `Access-Control-Allow-Origin` response header that matches the parent\n"
        <>  "import, otherwise Dhall rejects the import.\n"
        <>  "\n" <> prologue
      where
        prologue =
            case expectedOrigins of
                [ expectedOrigin ] ->
                        "The following parent import:\n"
                    <>  "\n"
                    <>  "↳ " <> show parentURL <> "\n"
                    <>  "\n"
                    <>  "... did not match the expected origin:\n"
                    <>  "\n"
                    <>  "↳ " <> show expectedOrigin <> "\n"
                    <>  "\n"
                    <>  "... so import resolution of the following child import failed:\n"
                    <>  "\n"
                    <>  "↳ " <> show childURL <> "\n"
                [] ->
                        "The child response did not include any `Access-Control-Allow-Origin` header,\n"
                    <>  "so resolution of the following import failed:\n"
                    <>  "\n"
                    <>  "↳ " <> show parentURL <> "\n"
                    <>  "\n"
                    <>  "Child import:\n"
                    <>  "\n"
                    <>  "↳ " <> show childURL <> "\n"
                _:_:_ ->
                        "The child response included more than one `Access-Control-Allow-Origin` header,\n"
                    <>  "when only one such header should have been present, so import resolution\n"
                    <>  "failed.\n"
                    <>  "\n"
                    <>  "This may indicate that the server for the child import is misconfigured.\n"
                    <>  "\n"
                    <>  "Parent import:\n"
                    <>  "\n"
                    <>  "↳ " <> show parentURL <> "\n"
                    <>  "\n"
                    <>  "Child import:\n"
                    <>  "\n"
                    <>  "↳ " <> show childURL <> "\n"
                    <>  "\n"
                    <>  "Expected origins:\n"
                    <>  concatMap (\o -> "\n↳ " <> show o <> "\n") expectedOrigins

corsCompliant
    :: MonadIO io
    => ImportType -> URL -> [(CI ByteString, ByteString)] -> io ()
corsCompliant (Remote parentURL) childURL responseHeaders = liftIO $ do
    let toOrigin (URL {..}) =
            Data.Text.Encoding.encodeUtf8 (prefix <> "://" <> authority)
          where
            prefix =
                case scheme of
                    HTTP  -> "http"
                    HTTPS -> "https"

    let actualOrigin = toOrigin parentURL

    let childOrigin = toOrigin childURL

    let predicate (header, _) = header == "Access-Control-Allow-Origin"

    let originHeaders = filter predicate responseHeaders

    let expectedOrigins = map snd originHeaders

    case expectedOrigins of
        [expectedOrigin]
            | expectedOrigin == "*" ->
                return ()
            | expectedOrigin == actualOrigin ->
                return ()
        _   | actualOrigin == childOrigin ->
                return ()
            | otherwise ->
                Control.Exception.throwIO (NotCORSCompliant {..})
corsCompliant _ _ _ = return ()

addHeaders :: OriginHeaders -> Maybe [HTTPHeader] -> HTTP.Request -> HTTP.Request
addHeaders originHeaders urlHeaders request =
    request { HTTP.requestHeaders = (filterHeaders urlHeaders) <> perOriginHeaders }
      where
        origin = decodeUtf8 (HTTP.host request) <> ":" <> Text.pack (show (HTTP.port request))
 
        perOriginHeaders = HashMap.lookupDefault [] origin originHeaders

        filterHeaders = foldMap (filter (not . overridden))

        overridden :: HTTPHeader -> Bool
        overridden (key, _value) = any (matchesKey key) perOriginHeaders

        matchesKey :: CI ByteString -> HTTPHeader -> Bool
        matchesKey key (candidate, _value) = key == candidate

fetchFromHttpUrlBytes
    :: URL -> Maybe [HTTPHeader] -> StateT Status IO ByteString
fetchFromHttpUrlBytes childURL mheaders = do
    Status { _loadOriginHeaders } <- State.get

    originHeaders <- _loadOriginHeaders

    manager <- newManager

    let childURLString = Text.unpack (renderURL childURL)

    baseRequest <- liftIO (HTTP.parseUrlThrow childURLString)

    let requestWithHeaders = addHeaders originHeaders mheaders baseRequest

    let io = HTTP.httpLbs requestWithHeaders manager

    let handler e = do
            let _ = e :: HttpException
            Control.Exception.throwIO (mkPrettyHttpException childURLString e)

    response <- liftIO (Control.Exception.handle handler io)

    Status {..} <- State.get

    case _stack of
        -- We ignore the first import in the stack since that is the same import
        -- as the `childUrl`
        _ :| Chained parentImport : _ -> do
            let parentImportType = importType (importHashed parentImport)

            corsCompliant parentImportType childURL (HTTP.responseHeaders response)
        _ -> do
            return ()

    return (ByteString.Lazy.toStrict (HTTP.responseBody response))

fetchFromHttpUrl :: URL -> Maybe [HTTPHeader] -> StateT Status IO Text.Text
fetchFromHttpUrl childURL mheaders = do
    bytes <- fetchFromHttpUrlBytes childURL mheaders

    case Data.Text.Encoding.decodeUtf8' bytes of
        Left  err  -> liftIO (Control.Exception.throwIO err)
        Right text -> return text

originHeadersFileExpr :: IO (Expr Src Import)
originHeadersFileExpr = do
    directoryStr <- getXdgDirectory XdgConfig "dhall"
    let components = map Text.pack (splitDirectories directoryStr)
    let directory = Directory (reverse components)
    let file = (File directory "headers.dhall")
    return (Embed (Import (ImportHashed Nothing (Local Absolute file)) Code))
