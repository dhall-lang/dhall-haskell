{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-| Convert Dhall to YAML

-}
module Dhall.Yaml
    ( Options(..)
    , Dhall.JSON.Yaml.defaultOptions
    , dhallToYaml
    , jsonToYaml
    ) where

import Data.ByteString      (ByteString)
import Data.Scientific      (floatingOrInteger)
import Data.Text            (Text)
import Dhall.JSON           (SpecialDoubleMode (..), codeToHeaderAndValue)
import Dhall.JSON.Yaml      (Options (..))
import Dhall.Parser         (Header (..))

import qualified Data.Aeson
import qualified Data.Aeson.Key     as Key
import qualified Data.Aeson.KeyMap  as KeyMap
import qualified Data.ByteString
import qualified Data.Char          as Char
import qualified Data.Text          as Text
import qualified Data.Text.Encoding
import qualified Data.Vector
import qualified Dhall
import qualified Dhall.JSON.Yaml
import qualified Yamlet
import qualified Yamlet.Syntax      as Syntax
import qualified Yamlet.Value       as Value

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

  let adapt (header, value) = (header, omission value)

  (Header comment, json) <- adapt <$> explaining (codeToHeaderAndValue conversion UseYAMLEncoding remoteImportMode mFilePath code)

  let suffix
          | preserveHeader = Data.Text.Encoding.encodeUtf8 comment
          | otherwise      = mempty

  let header =
          if noEdit
          then Dhall.JSON.Yaml.generatedCodeNotice <> suffix
          else suffix

  return $ header <> jsonToYaml json documents quoted

-- | Transform json representation into yaml
jsonToYaml
    :: Data.Aeson.Value
    -> Bool
    -> Bool
    -> ByteString
jsonToYaml json documents quoted =
  case (documents, json) of
    (True, Data.Aeson.Array elems) ->
      Data.ByteString.intercalate "\n"
        $ (("---\n" <>) . encodeJson) <$> Data.Vector.toList elems
    (True, value) -> "---\n" <> encodeJson value
    _ -> encodeJson json
  where
    encodeJson value =
        ensureNewline
            (Data.Text.Encoding.encodeUtf8
                (Syntax.renderSyntax Syntax.defaultRenderOptions
                    [Syntax.document (valueNode quoted value)]))

    ensureNewline bytes
        | Data.ByteString.null bytes = "\n"
        | Data.ByteString.last bytes == 10 = bytes
        | otherwise = bytes <> "\n"

valueNode :: Bool -> Data.Aeson.Value -> Syntax.Node
valueNode quoted = \case
    Data.Aeson.String text -> textNode quoted text
    Data.Aeson.Number scientific ->
        case floatingOrInteger scientific of
            Right (integer :: Integer) ->
                Syntax.plainNode (Text.pack (show integer))
            Left (double :: Double) ->
                Syntax.plainNode (Text.pack (show double))
    Data.Aeson.Bool True -> Syntax.plainNode "true"
    Data.Aeson.Bool False -> Syntax.plainNode "false"
    Data.Aeson.Null -> Syntax.plainNode "null"
    Data.Aeson.Array elements ->
        Syntax.sequenceNode (valueNode quoted <$> Data.Vector.toList elements)
    Data.Aeson.Object object ->
        Syntax.mappingNode
            [ (textNode quoted (Key.toText key), valueNode quoted value)
            | (key, value) <- KeyMap.toAscList object
            ]

-- | Quoting rules from @yaml-conversion.md@.
textNode :: Bool -> Text -> Syntax.Node
textNode quoted text
    | "\n" `Text.isInfixOf` text =
        Syntax.scalarNode Syntax.Literal text
    | quoted || Text.all isNumberOrDateRelated text || isBoolString =
        Syntax.scalarNode Syntax.SingleQuoted text
    | needsDoubleQuotes text =
        Syntax.scalarNode Syntax.DoubleQuoted text
    | otherwise =
        Syntax.plainNode text
  where
    -- For backwards compatibility with YAML 1.1, we need to add the following to the set of boolean values:
    -- https://yaml.org/type/bool.html
    isBoolString =
        Text.length text <= 5 &&
        Text.toLower text `elem` ["y", "yes", "n", "no", "true", "false", "on", "off"]
    isNumberOrDateRelated c = Char.isDigit c || c == '.' || c == 'e' || c == '-'

-- | The YAML 1.2 core schema double-quotes a string it would not read back as that string.
needsDoubleQuotes :: Text -> Bool
needsDoubleQuotes text =
    Text.null text
        || " " `Text.isPrefixOf` text
        || " " `Text.isSuffixOf` text
        || Text.any (not . isPlainChar) text
        || isAmbiguous text
  where
    isPlainChar c = Char.isAlphaNum c || c `elem` (" ~$^+=</;._\\" :: String)

    isAmbiguous plain =
        case Yamlet.decodeText @Value.Value (plain <> "\n") of
            Right (Value.String decoded) -> decoded /= plain
            Right _ -> True
            Left _ -> True
