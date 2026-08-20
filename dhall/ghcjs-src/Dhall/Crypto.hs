{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}

{-| Cryptographic utilities for GHC's JavaScript backend (and former GHCJS).
-}

module Dhall.Crypto (
      SHA256Digest(..)
    , sha256DigestFromByteString
    , sha256Hash
    , toString
    ) where

import Control.DeepSeq     (NFData)
import Data.ByteString     (ByteString)
import Data.Data           (Data)
import GHC.Generics        (Generic)
import GHC.JS.Prim         (JSVal, fromJSString, toJSString)
import System.IO.Unsafe    (unsafePerformIO)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as ByteString.Char8

-- | A SHA256 digest
newtype SHA256Digest = SHA256Digest { unSHA256Digest :: ByteString }
  deriving (Data, Eq, Generic, Ord, NFData)

instance Show SHA256Digest where
  show = toString

{-| Attempt to interpret a `ByteString` as a `SHA256Digest`, returning
    `Nothing` if the conversion fails
-}
sha256DigestFromByteString :: ByteString -> Maybe SHA256Digest
sha256DigestFromByteString bytes
  | ByteString.length bytes == 32 = Just (SHA256Digest bytes)
  | otherwise                     = Nothing

-- Browser: WebCrypto. Node (including the JS TH runner): Node's crypto module.
foreign import javascript interruptible
  "((s, $c) => {\
  \  var a = new Uint8Array(s.length);\
  \  for (var i = 0; i < s.length; i++) a[i] = s.charCodeAt(i) & 0xff;\
  \  var done = function (buf) {\
  \    $c(String.fromCharCode.apply(null, new Uint8Array(buf)));\
  \  };\
  \  if (typeof process === 'undefined') {\
  \    crypto.subtle.digest('SHA-256', a).then(done);\
  \  } else {\
  \    done(require('crypto').createHash('sha256').update(Buffer.from(a)).digest());\
  \  }\
  \})"
  js_sha256Hash :: JSVal -> IO JSVal

-- | Hash a `ByteString` and return the hash as a `SHA256Digest`
sha256Hash :: ByteString -> SHA256Digest
sha256Hash bytes
  | ByteString.length out == 32 = SHA256Digest out
  | otherwise = error "sha256Hash: didn't produce 32 bytes"
  where
    out =
      ByteString.Char8.pack $ fromJSString $ unsafePerformIO $
        js_sha256Hash (toJSString (ByteString.Char8.unpack bytes))

-- | 'String' representation of a 'SHA256Digest'
toString :: SHA256Digest -> String
toString (SHA256Digest bytes) = ByteString.Char8.unpack $ Base16.encode bytes
