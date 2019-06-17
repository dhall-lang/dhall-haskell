{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Dhall.YamlToDhall
  ( Options(..)
  , defaultOptions
  , YAMLCompileError(..)
  , dhallFromYaml
  ) where

import Data.ByteString (ByteString)

import Dhall.JSONToDhall
  ( CompileError(..)
  , Conversion(..)
  , defaultConversion
  , dhallFromJSON
  , resolveSchemaExpr
  , showCompileError
  , typeCheckSchemaExpr
  )

import Control.Exception (Exception, throwIO)
import Data.Text (Text)

import qualified Dhall.Core as Dhall

#if defined(ETA_VERSION)
import Dhall.Yaml.Eta ( yamlToJson, showYaml )
#else
import Data.Aeson (Value)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml
#endif

-- | Options to parametrize conversion
data Options = Options
    { schema     :: Text
    , conversion :: Conversion
    } deriving Show

defaultOptions :: Text -> Options
defaultOptions schema = Options {..}
  where conversion = defaultConversion

                        
data YAMLCompileError = YAMLCompileError CompileError

instance Show YAMLCompileError where
    show (YAMLCompileError e) = showCompileError "YAML" showYaml e

instance Exception YAMLCompileError


-- | Transform yaml representation into dhall
dhallFromYaml :: Options -> ByteString -> IO Text
dhallFromYaml Options{..} yaml = do

  value <- either (throwIO . userError) pure (yamlToJson yaml)

  expr <- typeCheckSchemaExpr YAMLCompileError =<< resolveSchemaExpr schema

  let dhall = dhallFromJSON conversion expr value

  either (throwIO . YAMLCompileError) (pure . Dhall.pretty) dhall


#if !defined(ETA_VERSION)
yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson =
  bimap Data.Yaml.prettyPrintParseException id . Data.Yaml.decodeEither'

showYaml :: Value -> String
showYaml value = BS8.unpack (Data.Yaml.encode value)
#endif

