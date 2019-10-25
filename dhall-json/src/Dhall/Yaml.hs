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
import Data.ByteString.Lazy (toStrict)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Vector
import qualified Dhall
import qualified Options.Applicative
#if defined(GPL)
import qualified Data.YAML.Aeson
import qualified Data.YAML as Y
import qualified Data.YAML.Event as YE
import qualified Data.YAML.Token as YT
import qualified Data.YAML.Schema as YS
import qualified Data.Text as Text
#else
import qualified Data.Aeson.Yaml
import qualified Data.ByteString.Lazy
#endif

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
          , conversion = NoConversion
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
  -> Maybe FilePath  -- ^ The source file path. If no path is given, imports
                     -- are resolved relative to the current directory.
  -> Text  -- ^ Input text.
  -> IO ByteString
dhallToYaml Options{..} mFilePath code = do
  
  let explaining = if explain then Dhall.detailed else id

  json <- omission <$> explaining (codeToValue conversion UseYAMLEncoding mFilePath code)

  return $ jsonToYaml json documents quoted

-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =
#if defined(GPL)
  case (documents, json) of
    (True, Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap (Data.ByteString.Lazy.toStrict. (Data.YAML.Aeson.encodeValue' schemaEncoder YT.UTF8). (:[]))
         $ Data.Vector.toList elems
    _ -> Data.ByteString.Lazy.toStrict (Data.YAML.Aeson.encodeValue' schemaEncoder YT.UTF8 [json])
  where
    unquotedStyle (Y.SStr s)
        | "\n" `Text.isInfixOf` s =
            Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
    unquotedStyle s =
        YS.schemaEncoderScalar Y.coreSchemaEncoder s

    quotedStyle (Y.SStr s)
        | "\n" `Text.isInfixOf` s =
            Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
        | otherwise =
            Right (YE.untagged, YE.SingleQuoted, s)
    quotedStyle s =
        YS.schemaEncoderScalar Y.coreSchemaEncoder s
    
    style = if quoted then quotedStyle else unquotedStyle

    schemaEncoder = YS.setScalarStyle style Y.coreSchemaEncoder
#else
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
#endif
