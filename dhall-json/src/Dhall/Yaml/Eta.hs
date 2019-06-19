{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dhall.Yaml.Eta ( jsonToYaml, showYaml, yamlToJson ) where

import Data.Bifunctor (bimap)
import Control.Exception (try)
import Data.ByteString (ByteString)
import Java
import Java.Exception
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.UTF8
import qualified Data.Vector


foreign import java unsafe "@static Utils.jsonToYaml" javaJsonToYaml
  :: String -> String

jsonToYaml :: Data.Aeson.Value -> Bool -> Bool -> ByteString
jsonToYaml json documents _quoted =

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

foreign import java unsafe "@static Utils.yamlToJson" javaYamlToJson
  :: String -> IO String

yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson =   (>>= Data.Aeson.eitherDecode)
             . bimap getExceptionMessage
                     (  Data.ByteString.Lazy.fromStrict
                      . Data.ByteString.UTF8.fromString)
             . javaTryYamlToJson
             . Data.ByteString.UTF8.toString

  where javaTryYamlToJson :: String -> Either JException String
        javaTryYamlToJson = unsafePerformIO . try . javaYamlToJson

        getExceptionMessage :: JException -> String
        getExceptionMessage ex =
          unsafePerformJava $ ex <.> getLocalizedMessage

showYaml :: Data.Aeson.Value -> String
showYaml value =
  Data.ByteString.UTF8.toString (jsonToYaml value False False)
