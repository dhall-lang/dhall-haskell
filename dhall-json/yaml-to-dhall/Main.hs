{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception
import           Control.Exception (SomeException)
import           Control.Monad (when)
import qualified Data.ByteString.Char8 as BSL8
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import qualified GHC.IO.Encoding
import qualified Options.Applicative as O
import           Options.Applicative (Parser, ParserInfo)
import qualified System.Exit
import qualified System.IO

import           Dhall.JSONToDhall (Conversion, parseConversion)
import           Dhall.YamlToDhall (Options(..), dhallFromYaml)

import qualified Paths_dhall_json as Meta

-- ---------------
-- Command options
-- ---------------

data CommandOptions = CommandOptions
    { version    :: Bool
    , schema     :: Text
    , conversion :: Conversion
    } deriving Show

-- | Command info and description
parserInfo :: ParserInfo CommandOptions
parserInfo = O.info
          (  O.helper <*> parseOptions)
          (  O.fullDesc
          <> O.progDesc "Populate Dhall value given its Dhall type (schema) from a YAML expression"
          )



-- | Parser for all the command arguments and options
parseOptions :: Parser CommandOptions
parseOptions = CommandOptions <$> parseVersion
                              <*> parseSchema
                              <*> parseConversion
  where
    parseSchema  =  O.strArgument
                 (  O.metavar "SCHEMA"
                 <> O.help "Dhall type expression (schema)"
                 )
    parseVersion =  O.switch
                 (  O.long "version"
                 <> O.short 'V'
                 <> O.help "Display version"
                 )

-- ----------
-- Main
-- ----------

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    CommandOptions{..} <- O.execParser parserInfo

    when version $ do
      putStrLn (showVersion Meta.version)
      System.Exit.exitSuccess

    handle $ do
        stdin <- BSL8.getContents

        Text.putStr =<< dhallFromYaml (Options schema conversion) stdin


handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
