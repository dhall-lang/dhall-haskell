{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| Convert Dhall to YAML via JSON

    Since JSON is only a subset of YAML, the functionality offered here is more
    limited than what the @dhall-yaml@ package can offer.
-}
module Dhall.JSON.Yaml
  ( Options(..)
  , parseDocuments
  , parseQuoted
  , defaultOptions
  , dhallToYaml
  , dhallToYamlWith
  , jsonToYaml
  ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.JSON (Conversion(..), SpecialDoubleMode(..))
import Options.Applicative (Parser)

import qualified Data.Aeson
import qualified Data.Aeson.Yaml
import qualified Data.ByteString.Lazy
import qualified Data.Vector
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.JSON
import qualified Options.Applicative

data Options = Options
    { explain    :: Bool
    , omission   :: Data.Aeson.Value -> Data.Aeson.Value
    , documents  :: Bool
    , quoted     :: Bool
    , conversion :: Conversion
    , file       :: Maybe FilePath
    , output     :: Maybe FilePath
    }

defaultOptions :: Options
defaultOptions =
  Options { explain = False
          , omission = id
          , documents = False
          , quoted = False
          , conversion = Dhall.JSON.defaultConversion
          , file = Nothing
          , output = Nothing
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
    -> Maybe FilePath
    -- ^ The source file path. If no path is given, imports
    -- are resolved relative to the current directory.
    -> Text
    -- ^ Input text.
    -> IO ByteString
dhallToYaml mpath =
    dhallToYamlWith Dhall.Context.empty Nothing mpath

dhallToYamlWith
    :: _
    -> Maybe _
    -> Options
    -> Maybe FilePath
    -- ^ The source file path. If no path is given, imports
    -- are resolved relative to the current directory.
    -> Text
    -- ^ Input text.
    -> IO ByteString
dhallToYamlWith Options{..} mpath code = do
    let explaining =
            if explain
                then Dhall.detailed
                else id

    json <-
        explaining
            (Dhall.JSON.codeToValueWith context normalizer conversion UseYAMLEncoding mpath code)

    return $ jsonToYaml (omission json) documents quoted

-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =
  Data.ByteString.Lazy.toStrict $ case (documents, json) of
    (True, Data.Aeson.Array elems)
      -> (if quoted
            then Data.Aeson.Yaml.encodeQuotedDocuments
            else Data.Aeson.Yaml.encodeDocuments
         ) (Data.Vector.toList elems)
    _ -> (if quoted
            then Data.Aeson.Yaml.encodeQuoted
            else Data.Aeson.Yaml.encode
         ) json
