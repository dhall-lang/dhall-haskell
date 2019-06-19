{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Yaml
  ( Options(..)
  , parseDocuments
  , parseQuoted
  , defaultOptions
  , dhallToYaml ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.JSON (Conversion(..), SpecialDoubleMode(..), codeToValue)
import Options.Applicative (Parser)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Vector
import qualified Dhall
import qualified Options.Applicative
#if defined(ETA_VERSION)
import Dhall.Yaml.Eta ( jsonToYaml )
#else
import qualified Data.Yaml
# if MIN_VERSION_yaml(0,10,2)
import qualified Data.Text
import qualified Text.Libyaml
# endif
#endif


data Options = Options
    { explain    :: Bool
    , omission   :: Data.Aeson.Value -> Data.Aeson.Value
    , documents  :: Bool
    , quoted     :: Bool
    , conversion :: Conversion
    }

defaultOptions :: Options
defaultOptions =
  Options { explain = False
          , omission = id
          , documents = False
          , quoted = False
          , conversion = NoConversion
          }

parseDocuments :: Parser Bool
parseDocuments =
  Options.Applicative.switch
            (   Options.Applicative.long "documents"
            <>  Options.Applicative.help "If given a Dhall list, output a document for every element"
            )

parseQuoted :: Parser Bool
parseQuoted =
  Options.Applicative.switch
            (   Options.Applicative.long "quoted"
            <>  Options.Applicative.help "Prevent from generating not quoted scalars"
            )
                           
{-| Convert a piece of Text carrying a Dhall inscription to an equivalent YAML ByteString
-}
dhallToYaml
  :: Options
  -> Text  -- ^ Describe the input for the sake of error location.
  -> Text  -- ^ Input text.
  -> IO ByteString
dhallToYaml Options{..} name code = do
  
  let explaining = if explain then Dhall.detailed else id

  json <- omission <$> explaining (codeToValue conversion UseYAMLEncoding name code)

  return $ jsonToYaml json documents quoted

#if !defined(ETA_VERSION)
-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =

  case (documents, json) of
    (True, Data.Yaml.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap (encodeYaml encodeOptions)
         $ Data.Vector.toList elems
    _ -> encodeYaml encodeOptions json

  where
# if !MIN_VERSION_yaml(0,10,2)
    encodeYaml = Data.Yaml.encode
# else
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
# endif
#endif
