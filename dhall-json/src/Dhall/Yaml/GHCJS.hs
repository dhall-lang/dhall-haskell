{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Dhall.Yaml.GHCJS where

import Data.ByteString (ByteString)
import Data.JSString (JSString)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.UTF8
import qualified Data.JSString
import qualified Data.Vector


foreign import javascript unsafe "jsonToYaml($1)" jsonToYaml_ :: JSString -> JSString

jsonToYaml :: Data.Aeson.Value -> Bool -> Bool -> ByteString
jsonToYaml json documents _quoted =

  case (documents, json) of
    (True,  Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap aesonToYaml
         $ Data.Vector.toList elems
    _ -> aesonToYaml json

  where aesonToYaml :: Data.Aeson.Value -> ByteString
        aesonToYaml =
              Data.ByteString.UTF8.fromString
          .   Data.JSString.unpack
          .   jsonToYaml_
          .   Data.JSString.pack
          .   Data.ByteString.UTF8.toString
          .   Data.ByteString.Lazy.toStrict
          .   Data.Aeson.encode
