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
#if defined(ETA_VERSION)
import Dhall.Yaml.Eta ( jsonToYaml )
#else
import qualified Data.YAML.Aeson as YAML
import qualified Data.YAML as Y
import qualified Data.YAML.Event as YE
import qualified Data.YAML.Token as YT
import qualified Data.Text as Text
#endif


data Options = Options
    { explain    :: Bool
    , omission   :: Data.Aeson.Value -> Data.Aeson.Value
    , documents  :: Bool
    , quoted     :: Bool
    , conversion :: Conversion
    , file       :: Maybe FilePath
    }

defaultOptions :: Options
defaultOptions =
  Options { explain = False
          , omission = id
          , documents = False
          , quoted = False
          , conversion = NoConversion
          , file = Nothing
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

#if !defined(ETA_VERSION)
-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =

  case (documents, json) of
    (True, Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap (Data.ByteString.Lazy.toStrict. (YAML.encodeValue' schemaEncoder YT.UTF8). (:[]))
         $ Data.Vector.toList elems
    _ -> Data.ByteString.Lazy.toStrict (YAML.encodeValue' schemaEncoder YT.UTF8 [json])

  where
    defaultSchemaEncoder = Y.setScalarStyle style Y.defaultSchemaEncoder

    defaultEncodeStr s = case () of
      ()
        | "\n" `Text.isInfixOf` s -> Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
        | Y.isAmbiguous Y.coreSchemaResolver s -> Right (YE.untagged, YE.SingleQuoted, s)
        | otherwise -> Right (YE.untagged, YE.Plain, s)

    style s = case s of
      Y.SNull         -> Right (YE.untagged, YE.Plain, "null")
      Y.SBool  bool   -> Right (YE.untagged, YE.Plain, Y.encodeBool bool)
      Y.SFloat double -> Right (YE.untagged, YE.Plain, Y.encodeDouble double)
      Y.SInt   int    -> Right (YE.untagged, YE.Plain, Y.encodeInt int)
      Y.SStr   text   -> defaultEncodeStr text
      Y.SUnknown t v  -> Right (t, YE.SingleQuoted, v)

    customStyle (Y.SStr s) = case () of
        ()
            | "\n" `Text.isInfixOf` s -> Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
            | otherwise -> Right (YE.untagged, YE.SingleQuoted, s)
    customStyle scalar =  (Y.schemaEncoderScalar defaultSchemaEncoder) scalar
    
    customSchemaEncoder = Y.setScalarStyle customStyle defaultSchemaEncoder
    
    schemaEncoder = if quoted 
        then customSchemaEncoder 
        else defaultSchemaEncoder
#endif
