{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.JSON.Compat where

import Data.ByteString (ByteString)
import Data.Yaml (Value)

import qualified Data.Yaml
#if MIN_VERSION_yaml(0,10,2)
import qualified Data.Text
import qualified Text.Libyaml
#endif

encodeYaml :: Bool -> Value -> ByteString
encodeYaml _quoted =
#if MIN_VERSION_yaml(0,10,2)
    Data.Yaml.encodeWith encodeOptions
  where
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

    encodeOptions = if _quoted
        then quotedOptions
        else Data.Yaml.defaultEncodeOptions
#else
    Data.Yaml.encode
#endif
