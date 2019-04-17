{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Dhall.Yaml.Eta (encode) where

import Data.ByteString (ByteString)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.UTF8
import qualified Data.Vector

foreign import java unsafe "@static Utils.jsonToYaml" jsonToYaml
  :: String -> String

encode :: Bool -> Data.Aeson.Value -> ByteString
encode multipleDocs json =

  case (multipleDocs, json) of
    (True,  Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap aesonToYaml
         $ Data.Vector.toList elems
    _ -> aesonToYaml json
    
  where aesonToYaml =
           Data.ByteString.UTF8.fromString 
         . jsonToYaml
         . Data.ByteString.UTF8.toString
         . Data.ByteString.Lazy.toStrict
         . Data.Aeson.encode
