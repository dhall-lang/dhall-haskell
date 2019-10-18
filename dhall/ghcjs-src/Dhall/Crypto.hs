{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}

{-| This module provides implementations of cryptographic utilities that only
    work for GHCJS
-}

module Dhall.Crypto (
      SHA256Digest(..)
    , sha256DigestFromByteString
    , sha256Hash
    ) where

import Control.DeepSeq (NFData)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Char8  as ByteString.Char8
import qualified GHCJS.Buffer           as Buffer

-- | A SHA256 digest
newtype SHA256Digest = SHA256Digest { unSHA256Digest :: ByteString }
  deriving (Eq, Generic, Ord, NFData, ByteArrayAccess)

instance Show SHA256Digest where
  show (SHA256Digest bytes) = ByteString.Char8.unpack $ convertToBase Base16 bytes

{-| Attempt to interpret a `ByteString` as a `SHA256Digest`, returning
    `Nothing` if the conversion fails
-}
sha256DigestFromByteString :: ByteString -> Maybe SHA256Digest
sha256DigestFromByteString bytes
  | ByteString.length bytes == 32 = Just $ SHA256Digest bytes
  | otherwise = Nothing

-- Use NodeJS' crypto module if there's a 'process' module, e.g. we're running
-- inside GHCJS' THRunner. If we're running in the browser, use the WebCrypto
-- interface.
foreign import javascript interruptible
  "if (typeof process === 'undefined') { \
  \  crypto.subtle.digest('SHA-256', $1).then($c) \
  \} else { \
  \  $c(require('crypto').createHash('sha256').update(Buffer.from($1)).digest().buffer) \
  \}"
  js_sha256Hash :: ArrayBuffer -> IO ArrayBuffer

byteStringToArrayBuffer :: ByteString -> ArrayBuffer
byteStringToArrayBuffer b =
  js_arrayBufferSlice offset len $ Buffer.getArrayBuffer buffer
  where
    (buffer, offset, len) = Buffer.fromByteString b

foreign import javascript unsafe "$3.slice($1, $1 + $2)"
  js_arrayBufferSlice :: Int -> Int -> ArrayBuffer -> ArrayBuffer

arrayBufferToByteString :: ArrayBuffer -> ByteString
arrayBufferToByteString =
  Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer

-- | Hash a `ByteString` and return the hash as a `SHA256Digest`
sha256Hash :: ByteString -> SHA256Digest
sha256Hash bytes
  | ByteString.length out == 32 = SHA256Digest out
  | otherwise = error "sha256Hash: didn't produce 32 bytes"
  where
    out =
      arrayBufferToByteString $
      unsafePerformIO $ js_sha256Hash (byteStringToArrayBuffer bytes)
