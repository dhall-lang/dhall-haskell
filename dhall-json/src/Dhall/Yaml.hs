{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Dhall.Yaml ( jsonToYaml ) where

#if defined(__GHCJS__)
import Dhall.Yaml.GHCJS ( jsonToYaml )
#elif defined(ETA)
import Dhall.Yaml.Eta ( jsonToYaml )
#else 
import Data.ByteString (ByteString)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Vector
import qualified Data.Text
import qualified Data.Yaml
import qualified Text.Libyaml

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
