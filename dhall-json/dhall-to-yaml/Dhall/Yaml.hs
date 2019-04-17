{-# LANGUAGE OverloadedStrings #-}
module Dhall.Yaml ( encode ) where

import Data.ByteString (ByteString)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Vector
import qualified Data.Yaml

encode :: Bool -> Data.Aeson.Value -> ByteString
encode multipleDocs json =
  case (multipleDocs, json) of
    (True, Data.Yaml.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap Data.Yaml.encode
         $ Data.Vector.toList elems
    _ -> Data.Yaml.encode json
