{-# LANGUAGE RecordWildCards #-}

module Dhall.YamlToDhall
  ( Options(..)
  , defaultOptions
  , YAMLCompileError(..)
  , Dhall.Import.Manager
  , Dhall.Import.defaultNewManager
  , dhallFromYaml
  , schemaFromYaml
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson        (Value)
import Data.ByteString   (ByteString)
import Data.Text         (Text)
import Data.Void         (Void)
import Dhall.Core        (Expr)
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

import qualified Data.ByteString.Char8 as BS8
import qualified Data.YAML.Aeson
import qualified Dhall.Import

-- | Options to parametrize conversion
data Options = Options
    { schema     :: Maybe Text
    , conversion :: Conversion
    } deriving Show

defaultOptions :: Maybe Text -> Options
defaultOptions schema = Options {..}
  where conversion = defaultConversion

data YAMLCompileError = YAMLCompileError CompileError

instance Show YAMLCompileError where
    show (YAMLCompileError e) = showCompileError "YAML" showYaml e

instance Exception YAMLCompileError


-- | Transform yaml representation into dhall
dhallFromYaml :: Options -> IO Dhall.Import.Manager -> ByteString -> IO (Expr Src Void)
dhallFromYaml Options{..} newManager yaml = do
  value <- either (throwIO . userError) pure (yamlToJson yaml)

  finalSchema <-
      case schema of
          Just text -> resolveSchemaExpr newManager text
          Nothing   -> return (schemaToDhallType (inferSchema value))

  expr <- typeCheckSchemaExpr YAMLCompileError finalSchema

  let dhall = dhallFromJSON conversion expr value

  either (throwIO . YAMLCompileError) pure dhall

-- | Infer the schema from YAML
schemaFromYaml :: ByteString -> IO (Expr Src Void)
schemaFromYaml yaml = do
    value <- either (throwIO . userError) pure (yamlToJson yaml)

    return (schemaToDhallType (inferSchema value))

{-| Wrapper around `Data.YAML.Aeson.decode1Strict` that renders the error
    message
-}
yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson s = case Data.YAML.Aeson.decode1Strict s of
    Right v         -> Right v
    Left (pos, err) -> Left (show pos ++ err)

showYaml :: Value -> String
showYaml value = BS8.unpack (Data.YAML.Aeson.encode1Strict value)
