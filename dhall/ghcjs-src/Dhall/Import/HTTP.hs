{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Import.HTTP where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Semigroup ((<>))

import qualified Data.Text as Text
import qualified JavaScript.XHR

import Dhall.Core (URL(..))
import Dhall.URL (renderURL)
import Dhall.Import.Types

fetchFromHttpUrl
    :: a
    -> URL
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT Status IO Text.Text
fetchFromHttpUrl _ childURL Nothing = do
    let childURLText = renderURL childURLString

    let childURLString = Text.unpack childURLText

    -- No need to add a CORS compliance check when using GHCJS.  The browser
    -- will already check the CORS compliance of the following XHR
    (statusCode, body) <- liftIO (JavaScript.XHR.get childURLText)

    case statusCode of
        200 -> return ()
        _   -> fail (childURLString <> " returned a non-200 status code: " <> show statusCode)

    return body
fetchFromHttpUrl _ _ _ = do
    fail "Dhall does not yet support custom headers when built using GHCJS"
