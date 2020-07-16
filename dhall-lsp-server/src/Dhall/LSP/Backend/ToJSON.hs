module Dhall.LSP.Backend.ToJSON (CompileError, toJSON) where

import Data.ByteString.Lazy    (toStrict)
import Data.Text               (Text)
import Data.Text.Encoding      (decodeUtf8)
import Dhall.JSON              as Dhall
import Dhall.LSP.Backend.Dhall

import qualified Data.Aeson.Encode.Pretty as Aeson

-- | Try to convert a given Dhall expression to JSON.
toJSON :: WellTyped -> Either CompileError Text
toJSON expr = fmap (decodeUtf8 . toStrict . Aeson.encodePretty' config)
                (Dhall.dhallToJSON $ fromWellTyped expr)
  where
    config = Aeson.Config
              { Aeson.confIndent = Aeson.Spaces 2
              , Aeson.confCompare = compare
              , Aeson.confNumFormat = Aeson.Generic
              , Aeson.confTrailingNewline = False }
