{-# LANGUAGE OverloadedStrings #-}

module Dhall.Import.HTTP
    ( fetchFromHttpUrl
    , fetchFromHttpUrlBytes
    , originHeadersFileExpr
    ) where

import Control.Monad.IO.Class           (MonadIO (..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString                  (ByteString)
import Data.CaseInsensitive             (CI)
import Dhall.Core                       (URL (..), Expr (..))
import Dhall.Import.Types               (Import, Status)
import Dhall.Parser                     (Src)
import Dhall.URL                        (renderURL)

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified JavaScript.XHR

fetchFromHttpUrl
    :: URL
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT Status IO Text.Text
fetchFromHttpUrl childURL Nothing = do
    let childURLText = renderURL childURL

    let childURLString = Text.unpack childURLText

    -- No need to add a CORS compliance check when using GHCJS.  The browser
    -- will already check the CORS compliance of the following XHR
    (statusCode, body) <- liftIO (JavaScript.XHR.get childURLText)

    case statusCode of
        200 -> return ()
        _   -> fail (childURLString <> " returned a non-200 status code: " <> show statusCode)

    return body
fetchFromHttpUrl _ _ =
    fail "Dhall does not yet support custom headers when built using GHCJS"

fetchFromHTTPUrlBytes
    :: URL
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT Status IO ByteString
fetchFromHTTPUrlBytes childUrl mheader = do
    text <- fetchFromHTTPUrl childUrl mheader
    return (Text.Encoding.encodeUtf8 text)

originHeadersFileExpr :: IO (Expr Src Import)
originHeadersFileExpr = return Missing
