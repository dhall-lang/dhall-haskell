{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Dhall.Parser.InputText (
    -- * Input text
      InputText
    , inputTextFromString
    , inputTextFromText
    , inputTextToText
    , inputTextFromByteString
    , inputTextFromByteStringIO
    , inputTextFromLazyByteString
    , inputTextFromLazyByteStringIO
    , readInputText
    -- * Input chunk
    , InputChunk
    , inputChunkToText
    , inputChunkFromText
    , inputChunkToString
    , inputChunkFromString
    ) where

import           Control.Exception          (throwIO)
import           Data.Coerce                (coerce)
import           Data.Data                  (Data)
import           Data.Proxy                 (Proxy (..))
import           Data.Semigroup             (Semigroup (..))
import           Data.String                (IsString (..))
import           Data.Text                  (Text)

import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Text.Megaparsec

-- | Input text. This type deliberately doesn't have almost any instances.
newtype InputText = InputText Text

instance IsString InputText where
    fromString = inputTextFromString

inputTextFromString :: String -> InputText
inputTextFromString = coerce Data.Text.pack

inputTextFromText :: Text -> InputText
inputTextFromText = InputText -- coerce -- to use data constructor at least once

inputTextToText :: InputText -> Text
inputTextToText = coerce

inputTextFromByteString
    :: Data.ByteString.ByteString
    -> Either Data.Text.Encoding.Error.UnicodeException InputText
inputTextFromByteString = coerce Data.Text.Encoding.decodeUtf8'

inputTextFromByteStringIO
    :: Data.ByteString.ByteString
    -> IO InputText
inputTextFromByteStringIO = either throwIO return . inputTextFromByteString

inputTextFromLazyByteString
    :: Data.ByteString.Lazy.ByteString
    -> Either Data.Text.Encoding.Error.UnicodeException InputText
inputTextFromLazyByteString =
    coerce Data.Text.Encoding.decodeUtf8' . Data.ByteString.Lazy.toStrict

inputTextFromLazyByteStringIO
    :: Data.ByteString.Lazy.ByteString
    -> IO InputText
inputTextFromLazyByteStringIO = either throwIO return . inputTextFromLazyByteString

readInputText :: FilePath -> IO InputText
readInputText path = Data.ByteString.readFile path >>= inputTextFromByteStringIO

instance Text.Megaparsec.Stream InputText where
    type Token  InputText = Char
    type Tokens InputText = InputChunk

    take1_            = coerce (Text.Megaparsec.take1_ :: Text -> Maybe (Char, Text))
    takeN_            = coerce (Text.Megaparsec.takeN_ :: Int -> Text -> Maybe (Text, Text))
    tokensToChunk _   = coerce (Text.Megaparsec.tokensToChunk (Proxy :: Proxy Text))
    chunkToTokens _   = coerce (Text.Megaparsec.chunkToTokens (Proxy :: Proxy Text))
    chunkLength _     = coerce (Text.Megaparsec.chunkLength (Proxy :: Proxy Text))
    chunkEmpty _      = coerce (Text.Megaparsec.chunkEmpty (Proxy :: Proxy Text))
    takeWhile_        = coerce (Text.Megaparsec.takeWhile_ :: (Char -> Bool) -> Text -> (Text,  Text))
    showTokens _      = coerce (Text.Megaparsec.showTokens (Proxy :: Proxy Text))
    reachOffset       = coerce (Text.Megaparsec.reachOffset :: Int -> Text.Megaparsec.PosState Text -> (Text.Megaparsec.SourcePos, String, Text.Megaparsec.PosState Text))
    reachOffsetNoLine = coerce (Text.Megaparsec.reachOffsetNoLine :: Int -> Text.Megaparsec.PosState Text -> (Text.Megaparsec.SourcePos, Text.Megaparsec.PosState Text))

newtype InputChunk = InputChunk Text
  deriving (Eq, Ord, Show, Data)

instance IsString InputChunk where
    fromString = inputChunkFromString 

instance Semigroup InputChunk where
    (<>) = coerce ((<>) :: Text -> Text -> Text)

inputChunkToText :: InputChunk -> Text
inputChunkToText = coerce

inputChunkFromText :: Text -> InputChunk
inputChunkFromText = coerce

inputChunkToString :: InputChunk -> String
inputChunkToString = coerce Data.Text.unpack

inputChunkFromString :: String -> InputChunk
inputChunkFromString = coerce Data.Text.pack
