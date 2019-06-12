module Dhall.LSP.Backend.ToJSON where

import qualified Dhall.JSON as Dhall
import qualified Data.Aeson.Encode.Pretty as Aeson

import Dhall.LSP.Backend.Dhall

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

-- | Try to convert the given Dhall file to JSON.
dhallToJSON :: FilePath -> Text -> IO (Maybe Text)
dhallToJSON path text = do
  mexpr <- runDhallSafe path text
  case mexpr of
    Just expr -> case Dhall.dhallToJSON expr of
      Right value -> do
        let config = Aeson.Config
              { Aeson.confIndent = Aeson.Spaces 2
              , Aeson.confCompare = compare
              , Aeson.confNumFormat = Aeson.Generic
              , Aeson.confTrailingNewline = False }
        return . Just . decodeUtf8 . toStrict $
          Aeson.encodePretty' config value
      _ -> return Nothing
    Nothing -> return Nothing
