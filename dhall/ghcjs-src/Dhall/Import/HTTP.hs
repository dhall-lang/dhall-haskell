{-# LANGUAGE JavaScriptFFI     #-}
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
import Dhall.Parser                     (Src)
import Dhall.URL                        (renderURL)
import GHC.JS.Prim                      (JSVal, fromJSString, toJSString)

import Dhall.Core
    ( Expr (..)
    , Import (..)
    , ImportHashed (..)
    , ImportMode (..)
    , ImportType (..)
    , URL (..)
    )
import Dhall.Import.Types (Status)

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text.Encoding

-- Returns "STATUS\\nBODY". An empty body still has the status line.
foreign import javascript interruptible
  "((url, $c) => {\
  \  fetch(url).then(function (r) {\
  \    return r.text().then(function (t) {\
  \      $c(String(r.status) + '\\n' + t);\
  \    });\
  \  }).catch(function (e) {\
  \    $c('0\\n' + String(e));\
  \  });\
  \})"
  js_fetch :: JSVal -> IO JSVal

fetchFromHttpUrl
    :: URL
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT Status IO Text.Text
fetchFromHttpUrl childURL Nothing = do
    let childURLText = renderURL childURL
    let childURLString = Text.unpack childURLText

    -- The browser enforces CORS; no extra check is required here.
    payload <- liftIO (fromJSString <$> js_fetch (toJSString childURLString))
    let (code, rest) = break (== '\n') payload
        body = case rest of
            []     -> ""
            (_:xs) -> xs
    case reads code of
        [(statusCode, "")] | statusCode == (200 :: Int) ->
            return (Text.pack body)
        [(statusCode, "")] ->
            fail (childURLString <> " returned a non-200 status code: " <> show statusCode)
        _ ->
            fail (childURLString <> " fetch failed: " <> payload)
fetchFromHttpUrl _ _ =
    fail "Dhall does not yet support custom headers when built for JavaScript"

fetchFromHttpUrlBytes
    :: URL
    -> Maybe [(CI ByteString, ByteString)]
    -> StateT Status IO ByteString
fetchFromHttpUrlBytes childUrl mheader = do
    text <- fetchFromHttpUrl childUrl mheader
    return (Text.Encoding.encodeUtf8 text)

originHeadersFileExpr :: IO (Expr Src Import)
originHeadersFileExpr =
    return (Embed (Import (ImportHashed Nothing Missing) Code))
