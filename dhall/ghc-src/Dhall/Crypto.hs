{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| This module provides implementations of cryptographic utilities that only
    work for GHC (as opposed to GHCJS)
-}

module Dhall.Crypto (
      SHA256Digest(..)
    , sha256DigestFromByteString
    , sha256Hash
    , toString
    ) where

import Control.DeepSeq         (NFData)
import Data.ByteString         (ByteString)
import Data.Data               (Data)
import GHC.Generics            (Generic)

import qualified Crypto.Hash.SHA256
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

-- | Hash a `ByteString` and return the hash as a `SHA256Digest`
sha256Hash :: ByteString -> SHA256Digest
sha256Hash = SHA256Digest . Crypto.Hash.SHA256.hash

-- | 'String' representation of a 'SHA256Digest'
toString :: SHA256Digest -> String
toString (SHA256Digest bytes) = ByteString.Char8.unpack $ Base16.encode bytes
