{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Dhall.Import.HTTP where

import Control.Exception (throwIO)
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
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding

import Dhall.Import.Types

#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client
    (HttpException(..), HttpExceptionContent(..), Manager)
#else
import Network.HTTP.Client (HttpException(..), Manager)
#endif

import qualified Network.HTTP.Client                     as HTTP
import qualified Network.HTTP.Client.TLS                 as HTTP

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
    ConnectionFailure e' ->
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

needManager :: StateT Status IO Manager
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
    -> StateT Status IO (String, Text.Text)
fetchFromHttpUrl url mheaders = do
    m <- needManager

    request <- liftIO (HTTP.parseUrlThrow url)

    let requestWithHeaders =
            case mheaders of
              Nothing      -> request
              Just headers -> request { HTTP.requestHeaders = headers }

    response <- liftIO (HTTP.httpLbs requestWithHeaders m)

    let bytes = HTTP.responseBody response

    case Data.Text.Lazy.Encoding.decodeUtf8' bytes of
        Left  err  -> liftIO (throwIO err)
        Right text -> return (url, Data.Text.Lazy.toStrict text)
