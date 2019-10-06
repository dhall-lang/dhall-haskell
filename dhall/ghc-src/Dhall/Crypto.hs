{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Dhall.Crypto (
      SHA256Digest(..)
    , sha256DigestFromByteString
    , sha256Hash
    ) where

import Control.DeepSeq (NFData)
import "cryptonite" Crypto.Hash (SHA256) -- To avoid conflict with "hashing"
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import qualified "cryptonite" Crypto.Hash -- To avoid conflict with "hashing"
import qualified Data.ByteString.Char8 as ByteString.Char8

newtype SHA256Digest = SHA256Digest { unSHA256Digest :: ByteString }
  deriving (Eq, Generic, Ord, NFData, ByteArrayAccess)

instance Show SHA256Digest where
  show (SHA256Digest bytes) = ByteString.Char8.unpack $ convertToBase Base16 bytes

sha256DigestFromByteString :: ByteString -> Maybe SHA256Digest
sha256DigestFromByteString bytes = SHA256Digest . convert <$> mh
  where
    mh = Crypto.Hash.digestFromByteString bytes :: Maybe (Crypto.Hash.Digest SHA256)

sha256Hash :: ByteString -> SHA256Digest
sha256Hash bytes = SHA256Digest $ convert h
  where
    h = Crypto.Hash.hash bytes :: Crypto.Hash.Digest SHA256
