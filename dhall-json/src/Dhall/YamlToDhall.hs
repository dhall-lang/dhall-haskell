{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Dhall.YamlToDhall
  ( Options(..)
  , defaultOptions
  , YAMLCompileError(..)
  , dhallFromYaml
  ) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.YAML.Aeson
import Dhall.Core (Expr)
import Dhall.JSONToDhall
  ( CompileError(..)
  , Conversion(..)
  , defaultConversion
  , dhallFromJSON
  , resolveSchemaExpr
  , showCompileError
  , typeCheckSchemaExpr
  )
import Dhall.Src (Src)

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
dhallFromYaml :: Options -> ByteString -> IO (Expr Src Void)
dhallFromYaml Options{..} yaml = do

  value <- either (throwIO . userError) pure (yamlToJson yaml)

  expr <- typeCheckSchemaExpr YAMLCompileError =<< resolveSchemaExpr schema

  let dhall = dhallFromJSON conversion expr value

  either (throwIO . YAMLCompileError) pure dhall


yamlToJson :: ByteString -> Either String Data.Aeson.Value
yamlToJson s = case Data.YAML.Aeson.decode1Strict s of
                  Right v -> Right v
                  Left (pos, err) -> Left (show pos ++ err)

showYaml :: Value -> String
showYaml value = BS8.unpack (Data.YAML.Aeson.encode1Strict value)
