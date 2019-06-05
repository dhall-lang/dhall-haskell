{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Dhall.Yaml ( jsonToYaml, yamlToJson ) where

#if defined(ETA_VERSION)
import Dhall.Yaml.Eta ( jsonToYaml , yamlToJson )
#else 
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Vector
import qualified Data.Yaml
#if MIN_VERSION_yaml(0,10,2)
import qualified Data.Text
import qualified Text.Libyaml
#endif

-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted = case (documents, json) of
  (True, Data.Yaml.Array elems)
    -> Data.ByteString.intercalate "\n---\n"
       $ fmap (encodeYaml encodeOptions)
       $ Data.Vector.toList elems
  _ -> encodeYaml encodeOptions json
  where
#if !MIN_VERSION_yaml(0,10,2)
    encodeYaml = Data.Yaml.encode
#else
    encodeYaml = Data.Yaml.encodeWith

    customStyle = \s -> case () of
        ()
            | "\n" `Data.Text.isInfixOf` s -> ( noTag, literal )
            | otherwise -> ( noTag, Text.Libyaml.SingleQuoted )
        where
            noTag = Text.Libyaml.NoTag
            literal = Text.Libyaml.Literal

    quotedOptions = Data.Yaml.setStringStyle
                        customStyle
                        Data.Yaml.defaultEncodeOptions

    encodeOptions = if quoted
        then quotedOptions
        else Data.Yaml.defaultEncodeOptions
#endif

-- | Transform yaml representation into dhall
yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson  =
  bimap Data.Yaml.prettyPrintParseException id . Data.Yaml.decodeEither'

#endif
