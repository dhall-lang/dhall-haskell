{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Dhall.YamlToDhall
  ( Options(..)
  , defaultOptions
  , YAMLCompileError(..)
  , dhallFromYaml
  , schemaFromYaml
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson        (Value)
import Data.ByteString   (ByteString)
import Data.List         (foldl')
import Data.Text         (Text)
import Data.Void         (Void)
import Dhall.Core        (Expr)
import Dhall.Import (RemoteImportMode (..))
import Dhall.JSONToDhall
    ( CompileError (..)
    , Conversion (..)
    , defaultConversion
    , dhallFromJSON
    , inferSchema
    , resolveSchemaExpr
    , schemaToDhallType
    , showCompileError
    , typeCheckSchemaExpr
    )
import Dhall.Src         (Src)

import qualified Data.Aeson         as Aeson
import qualified Data.Aeson.Key     as Key
import qualified Data.Aeson.KeyMap  as KeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Scientific    as Scientific
import qualified Data.Vector        as Vector
import qualified Dhall.Yaml
import qualified Yamlet
import qualified Yamlet.Value       as Value

-- | Options to parametrize conversion
data Options = Options
    { schema     :: Maybe Text
    , conversion :: Conversion
    , remoteImportMode :: RemoteImportMode
    } deriving Show

defaultOptions :: Maybe Text -> Options
defaultOptions schema = Options {..}
  where
    conversion = defaultConversion
    remoteImportMode = AllowRemoteImports

data YAMLCompileError = YAMLCompileError CompileError

instance Show YAMLCompileError where
    show (YAMLCompileError e) = showCompileError "YAML" showYaml e

instance Exception YAMLCompileError


-- | Transform yaml representation into dhall
dhallFromYaml :: Options -> ByteString -> IO (Expr Src Void)
dhallFromYaml Options{..} yaml = do
  value <- either (throwIO . userError) pure (yamlToJson yaml)

  finalSchema <-
      case schema of
          Just text -> resolveSchemaExpr remoteImportMode text
          Nothing   -> return (schemaToDhallType (inferSchema value))

  expr <- typeCheckSchemaExpr YAMLCompileError finalSchema

  let dhall = dhallFromJSON conversion expr value

  either (throwIO . YAMLCompileError) pure dhall

-- | Infer the schema from YAML
schemaFromYaml :: ByteString -> IO (Expr Src Void)
schemaFromYaml yaml = do
    value <- either (throwIO . userError) pure (yamlToJson yaml)

    return (schemaToDhallType (inferSchema value))

{-| Decode one YAML 1.2 document into a JSON value.

    A stream with any other number of documents is rejected. A tag outside
    the core schema is rejected. Two equal keys are an error.
-}
yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson bytes =
    case Yamlet.decodeAll @Value.Value bytes of
        Left errs ->
            Left (unlines (Yamlet.prettyError "" <$> NonEmpty.toList errs))
        Right [value] -> valueToAeson value
        Right [] ->
            Left "No documents found in YAML stream"
        Right _ ->
            Left "Multiple documents encountered in YAML stream"

valueToAeson :: Value.Value -> Either String Data.Aeson.Value
valueToAeson = \case
    Value.Null -> Right Aeson.Null
    Value.Bool bool -> Right (Aeson.Bool bool)
    Value.Int integer -> Right (Aeson.Number (fromInteger integer))
    Value.Float (Value.Finite scientific) -> Right (Aeson.Number scientific)
    Value.Float Value.NegativeZero ->
        Right (Aeson.Number (doubleToScientific (-0)))
    Value.Float Value.Infinity ->
        Right (Aeson.Number (doubleToScientific (1 / 0)))
    Value.Float Value.NegativeInfinity ->
        Right (Aeson.Number (doubleToScientific ((-1) / 0)))
    Value.Float Value.NaN ->
        -- The sign bit of a quiet NaN depends on the platform. YAML `.nan`
        -- has no sign, so the integer is the positive magnitude.
        Right (Aeson.Number (abs (doubleToScientific (0 / 0))))
    Value.String text -> Right (Aeson.String text)
    Value.Sequence values ->
        Aeson.Array . Vector.fromList <$> traverse valueToAeson values
    Value.Mapping pairs -> do
        entries <- traverse mappingEntry pairs
        pure (Aeson.Object (foldl' insert KeyMap.empty entries))
    Value.Tagged _ _ ->
        Left "unresolved YAML scalar encountered"
  where
    mappingEntry (key, value) = do
        text <- case key of
            Value.String text -> Right text
            _ -> Left "non-String key encountered in mapping"
        decoded <- valueToAeson value
        pure (Key.fromText text, decoded)

    insert object (key, value) = KeyMap.insert key value object

doubleToScientific :: Double -> Scientific.Scientific
doubleToScientific = realToFrac

showYaml :: Value -> String
showYaml value = BS8.unpack (Dhall.Yaml.jsonToYaml value False False)
