{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Import.HTTP where

import Control.Exception (Exception)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Lens.Family.State.Strict (zoom)

import Dhall.Core
    ( Directory(..)
    , File(..)
    , Import(..)
    , ImportHashed(..)
    , ImportType(..)
    , Scheme(..)
    , URL(..)
    )

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding
import qualified Dhall.Util
import qualified Network.URI.Encode               as URI.Encode

import Dhall.Import.Types

import qualified Control.Exception
#ifdef __GHCJS__
import qualified JavaScript.XHR
#else
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
#endif

#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client
    (HttpException(..), HttpExceptionContent(..), Manager)
#else
import Network.HTTP.Client (HttpException(..), Manager)
#endif

import qualified Network.HTTP.Client                     as HTTP
import qualified Network.HTTP.Client.TLS                 as HTTP
import qualified Network.HTTP.Types.Status

mkPrettyHttpException :: HttpException -> PrettyHttpException
mkPrettyHttpException ex =
    PrettyHttpException (renderPrettyHttpException ex) (toDyn ex)

renderPrettyHttpException :: HttpException -> String
#if MIN_VERSION_http_client(0,5,0)
renderPrettyHttpException (InvalidUrlException _ r) =
  "\n"
  <>  "\ESC[1;31mError\ESC[0m: Invalid URL\n"
  <>  "\n"
  <>  "↳ " <> show r
renderPrettyHttpException (HttpExceptionRequest _ e) =
  case e of
    ConnectionFailure _ ->
      "\n"
      <>  "\ESC[1;31mError\ESC[0m: Remote host not found\n"
    InvalidDestinationHost host ->
      "\n"
      <>  "\ESC[1;31mError\ESC[0m: Invalid remote host name\n"
      <>  "\n"
      <>  "↳ " <> show host
    ResponseTimeout ->
      "\n"
      <>  "\ESC[1;31mError\ESC[0m: The remote host took too long to respond"
    StatusCodeException response _
        | statusCode == 404 ->
            "\n"
            <>  "\ESC[1;31mError\ESC[0m: Remote file not found"
        | otherwise ->
            "\n"
            <>  "\ESC[1;31mError\ESC[0m: Unexpected HTTP status code:\n"
            <>  "\n"
            <>  "↳ " <> show statusCode
      where
        statusCode =
            Network.HTTP.Types.Status.statusCode
                (HTTP.responseStatus response)
    e' -> "\n" <> show e'
#else
renderPrettyHttpException e = case e of
    FailedConnectionException2 _ _ _ e' ->
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Wrong host\n"
        <>  "\n"
        <>  "↳ " <> show e'
    InvalidDestinationHost host ->
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Invalid host name\n"
        <>  "\n"
        <>  "↳ " <> show host
    ResponseTimeout ->
            "\ESC[1;31mError\ESC[0m: The host took too long to respond\n"
    e' ->   "\n"
        <> show e'
#endif

needManager :: StateT (Status m) IO Manager
needManager = do
    x <- zoom manager State.get
    case join (fmap fromDynamic x) of
        Just m  -> return m
        Nothing -> do
            let settings = HTTP.tlsManagerSettings

#ifdef MIN_VERSION_http_client
#if MIN_VERSION_http_client(0,5,0)
                    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (30 * 1000 * 1000) }  -- 30 seconds
#else
                    { HTTP.managerResponseTimeout = Just (30 * 1000 * 1000) }  -- 30 seconds
#endif
#endif
            m <- liftIO (HTTP.newManager settings)
            zoom manager (State.put (Just (toDyn m)))
            return m

data NotCORSCompliant = NotCORSCompliant
    { expectedOrigins :: [ByteString]
    , actualOrigin    :: ByteString
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
                    <>  "↳ " <> show actualOrigin <> "\n"
                    <>  "\n"
                    <>  "... did not match the expected origin:\n"
                    <>  "\n"
                    <>  "↳ " <> show expectedOrigin <> "\n"
                    <>  "\n"
                    <>  "... so import resolution failed.\n"
                [] ->
                        "The child response did not include any `Access-Control-Allow-Origin` header,\n"
                    <>  "so import resolution failed.\n"
                _:_:_ ->
                        "The child response included more than one `Access-Control-Allow-Origin` header,\n"
                    <>  "when only one such header should have been present, so import resolution\n"
                    <>  "failed.\n"
                    <>  "\n"
                    <>  "This may indicate that the server for the child import is misconfigured.\n"

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

renderComponent :: Text -> Text
renderComponent component = "/" <> URI.Encode.encodeText component

renderQuery :: Text -> Text
renderQuery query = "?" <> query

renderURL :: URL -> Text
renderURL url =
        schemeText
    <>  authority
    <>  pathText
    <>  queryText
  where
    URL {..} = url

    File {..} = path

    Directory {..} = directory

    schemeText = case scheme of
        HTTP  -> "http://"
        HTTPS -> "https://"

    pathText =
            foldMap renderComponent (reverse components)
        <>  renderComponent file

    queryText = foldMap renderQuery query

fetchFromHttpUrl
    :: URL
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT (Status m) IO (String, Text.Text)
#ifdef __GHCJS__
fetchFromHttpUrl childURL Nothing = do
    let childURLText = renderURL childURL

    let childURLString = Text.unpack childURLText

    -- No need to add a CORS compliance check when using GHCJS.  The browser
    -- will already check the CORS compliance of the following XHR
    (statusCode, body) <- liftIO (JavaScript.XHR.get childURLText)

    case statusCode of
        200 -> return ()
        _   -> fail (childURLString <> " returned a non-200 status code: " <> show statusCode)

    return (childURLString, body)
fetchFromHttpUrl _ _ = do
    fail "Dhall does not yet support custom headers when built using GHCJS"
#else
fetchFromHttpUrl childURL mheaders = do
    let childURLString = Text.unpack (renderURL childURL)

    m <- needManager

    request <- liftIO (HTTP.parseUrlThrow childURLString)

    let requestWithHeaders =
            case mheaders of
              Nothing      -> request
              Just headers -> request { HTTP.requestHeaders = headers }

    let io = HTTP.httpLbs requestWithHeaders m

    let handler e = do
            let _ = e :: HttpException
            Control.Exception.throwIO (mkPrettyHttpException e)

    response <- liftIO (Control.Exception.handle handler io)

    Status {..} <- State.get

    let parentImport = NonEmpty.head _stack

    let parentImportType = importType (importHashed parentImport)

    corsCompliant parentImportType childURL (HTTP.responseHeaders response)

    let bytes = HTTP.responseBody response

    case Data.Text.Lazy.Encoding.decodeUtf8' bytes of
        Left  err  -> liftIO (Control.Exception.throwIO err)
        Right text -> return (childURLString, Data.Text.Lazy.toStrict text)
#endif
