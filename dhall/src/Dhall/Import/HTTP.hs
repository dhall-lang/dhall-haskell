{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Dhall.Import.HTTP where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Dynamic (fromDynamic, toDyn)
import Data.Semigroup ((<>))
import Lens.Family.State.Strict (zoom)

import qualified Control.Monad.Trans.State.Strict        as State
import qualified Data.Text                               as Text

import Dhall.Import.Types

#ifdef __GHCJS__
import qualified JavaScript.XHR
#else
import qualified Control.Exception
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
      <>  "↳ " <> show host <> "\n"
    ResponseTimeout ->
      "\n"
      <>  "\ESC[1;31mError\ESC[0m: The remote host took too long to respond\n"
    StatusCodeException response _
        | statusCode == 404 ->
            "\n"
            <>  "\ESC[1;31mError\ESC[0m: Remote file not found\n"
        | otherwise ->
            "\n"
            <>  "\ESC[1;31mError\ESC[0m: Unexpected HTTP status code:\n"
            <>  "\n"
            <>  "↳ " <> show statusCode <> "\n"
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

fetchFromHttpUrl
    :: String
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT (Status m) IO (String, Text.Text)
#ifdef __GHCJS__
fetchFromHttpUrl url Nothing = do
    (statusCode, body) <- liftIO (JavaScript.XHR.get (Text.pack url))

    case statusCode of
        200 -> return ()
        _   -> fail (url <> " returned a non-200 status code: " <> show statusCode)

    return (url, body)
fetchFromHttpUrl _ _ = do
    fail "Dhall does not yet support custom headers when built using GHCJS"
#else
fetchFromHttpUrl url mheaders = do
    m <- needManager

    request <- liftIO (HTTP.parseUrlThrow url)

    let requestWithHeaders =
            case mheaders of
              Nothing      -> request
              Just headers -> request { HTTP.requestHeaders = headers }

    let io = HTTP.httpLbs requestWithHeaders m

    let handler e = do
            let _ = e :: HttpException
            Control.Exception.throwIO (mkPrettyHttpException e)

    response <- liftIO (Control.Exception.handle handler io)

    let bytes = HTTP.responseBody response

    case Data.Text.Lazy.Encoding.decodeUtf8' bytes of
        Left  err  -> liftIO (Control.Exception.throwIO err)
        Right text -> return (url, Data.Text.Lazy.toStrict text)
#endif
