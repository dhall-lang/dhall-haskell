{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Dhall.Yaml.Eta ( jsonToYaml ) where

import Data.ByteString (ByteString)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.UTF8
import qualified Data.Vector

foreign import java unsafe "@static Utils.jsonToYaml" javaJsonToYaml
  :: String -> String

jsonToYaml :: Data.Aeson.Value -> Bool -> Bool -> ByteString
encode json documents quoted =

  case (documents, json) of
    (True,  Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap aesonToYaml
         $ Data.Vector.toList elems
    _ -> aesonToYaml json
    
  where aesonToYaml =
           Data.ByteString.UTF8.fromString 
         . javaJsonToYaml
         . Data.ByteString.UTF8.toString
         . Data.ByteString.Lazy.toStrict
         . Data.Aeson.encode
