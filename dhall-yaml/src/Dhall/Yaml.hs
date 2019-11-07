{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-| Convert Dhall to YAML

-}
module Dhall.Yaml
    ( Options(..)
    , Dhall.JSON.Yaml.defaultOptions
    , dhallToYaml
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Dhall.JSON (SpecialDoubleMode(..), codeToValue)
import Dhall.JSON.Yaml (Options(..))
import Data.ByteString.Lazy (toStrict)

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Text as Text
import qualified Data.Vector
import qualified Data.YAML as Y
import qualified Data.YAML.Aeson
import qualified Data.YAML.Event as YE
import qualified Data.YAML.Schema as YS
import qualified Data.YAML.Token as YT
import qualified Dhall
import qualified Dhall.JSON.Yaml

{-| Convert a piece of 'Text' carrying a Dhall inscription to an equivalent @YAML@ 'ByteString'
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
  case (documents, json) of
    (True, Data.Aeson.Array elems)
      -> Data.ByteString.intercalate "\n---\n"
         $ fmap (Data.ByteString.Lazy.toStrict. (Data.YAML.Aeson.encodeValue' schemaEncoder YT.UTF8). (:[]))
         $ Data.Vector.toList elems
    _ -> Data.ByteString.Lazy.toStrict (Data.YAML.Aeson.encodeValue' schemaEncoder YT.UTF8 [json])
  where
    style (Y.SStr s)
        | "\n" `Text.isInfixOf` s =
            Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
        | quoted =
            Right (YE.untagged, YE.SingleQuoted, s)
    style s =
        YS.schemaEncoderScalar Y.coreSchemaEncoder s

    schemaEncoder = YS.setScalarStyle style Y.coreSchemaEncoder
