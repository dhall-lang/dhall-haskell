{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-| Convert Dhall to YAML

-}
module Dhall.Yaml
    ( Options(..)
    , Dhall.JSON.Yaml.defaultOptions
    , Dhall.Import.Manager
    , Dhall.Import.defaultNewManager
    , dhallToYaml
    ) where

import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text            (Text)
import Dhall.JSON           (SpecialDoubleMode (..), codeToValue)
import Dhall.JSON.Yaml      (Options (..))

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Char        as Char
import qualified Data.Text        as Text
import qualified Data.Vector
import qualified Data.YAML        as Y
import qualified Data.YAML.Aeson
import qualified Data.YAML.Event  as YE
import qualified Data.YAML.Schema as YS
import qualified Data.YAML.Token  as YT
import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.JSON.Yaml

{-| Convert a piece of 'Text' carrying a Dhall inscription to an equivalent @YAML@ 'ByteString'
-}
dhallToYaml
  :: Options
  -> IO Dhall.Import.Manager
  -> Maybe FilePath  -- ^ The source file path. If no path is given, imports
                     -- are resolved relative to the current directory.
  -> Text  -- ^ Input text.
  -> IO ByteString
dhallToYaml Options{..} newManager mFilePath code = do

  let explaining = if explain then Dhall.detailed else id

  json <- omission <$> explaining (codeToValue conversion UseYAMLEncoding newManager mFilePath code)

  let header =
          if noEdit
          then Dhall.JSON.Yaml.generatedCodeNotice
          else mempty

  return $ header <> jsonToYaml json documents quoted

-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =
  case (documents, json) of
    (True, Data.Aeson.Array elems) -> document elems
    (True, value) -> document (pure value)
    _ -> Data.ByteString.Lazy.toStrict (encoder [json])
  where
    document elems =
      Data.ByteString.intercalate "\n"
         $ (("---\n" <>) . Data.ByteString.Lazy.toStrict . encoder . (:[])) <$> Data.Vector.toList elems

    style (Y.SStr s)
        | "\n" `Text.isInfixOf` s =
            Right (YE.untagged, YE.Literal YE.Clip YE.IndentAuto, s)
        | quoted || Text.all isNumberOrDateRelated s || isBoolString =
            Right (YE.untagged, YE.SingleQuoted, s)
      where
        -- For backwards compatibility with YAML 1.1, we need to add the following to the set of boolean values:
        -- https://yaml.org/type/bool.html
        isBoolString = Text.length s <= 5 &&
                      Text.toLower s `elem` ["y", "yes", "n", "no", "true", "false", "on", "off"]
        isNumberOrDateRelated c = Char.isDigit c || c == '.' || c == 'e' || c == '-'
    style s =
        YS.schemaEncoderScalar Y.coreSchemaEncoder s

    schemaEncoder = YS.setScalarStyle style Y.coreSchemaEncoder
    encoder = Data.YAML.Aeson.encodeValue' schemaEncoder YT.UTF8
