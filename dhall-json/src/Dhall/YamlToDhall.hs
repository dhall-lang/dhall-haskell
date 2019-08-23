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
import Dhall.Core (Expr)
import Dhall.Src (Src)
import Dhall.TypeCheck(X)

#if defined(ETA_VERSION)
import Dhall.Yaml.Eta ( yamlToJson, showYaml )
#else
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.YAML.Aeson
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
dhallFromYaml :: Options -> ByteString -> IO (Expr Src X)
dhallFromYaml Options{..} yaml = do

  value <- either (throwIO . userError) pure (yamlToJson yaml)

  expr <- typeCheckSchemaExpr YAMLCompileError =<< resolveSchemaExpr schema

  let dhall = dhallFromJSON conversion expr value

  either (throwIO . YAMLCompileError) pure dhall


#if !defined(ETA_VERSION)
yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson = Data.YAML.Aeson.decode1Strict

showYaml :: Value -> String
showYaml value = BS8.unpack (Data.YAML.Aeson.encode1Strict value)
#endif

