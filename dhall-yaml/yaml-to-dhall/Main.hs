{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (optional, (<|>))
import Control.Exception   (SomeException)
import Data.Text           (Text)
import Data.Version        (showVersion)
import Dhall.JSONToDhall   (Conversion, parseConversion)
import Dhall.Pretty        (CharacterSet (..))
import Dhall.Util          (renderExpression)
import Dhall.YamlToDhall   (Options (..), dhallFromYaml)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.ByteString.Char8                     as BSL8
import qualified Dhall.YamlToDhall                         as YamlToDhall
import qualified GHC.IO.Encoding
import qualified Options.Applicative                       as Options
import qualified Paths_dhall_yaml                          as Meta
import qualified System.Exit
import qualified System.IO                                 as IO

-- ---------------
-- Command options
-- ---------------

data CommandOptions
    = Default
        { schema     :: Maybe Text
        , conversion :: Conversion
        , file       :: Maybe FilePath
        , output     :: Maybe FilePath
        , ascii      :: Bool
        , plain      :: Bool
        }
    | Type
        { file       :: Maybe FilePath
        , output     :: Maybe FilePath
        , ascii      :: Bool
        , plain      :: Bool
        }
    | Version
    deriving (Show)

-- | Command info and description
parserInfo :: ParserInfo CommandOptions
parserInfo = Options.info
          (  Options.helper <*> parseOptions)
          (  Options.fullDesc
          <> Options.progDesc "Convert a YAML expression to a Dhall expression, given the expected Dhall type"
          )

-- | Parser for all the command arguments and options
parseOptions :: Parser CommandOptions
parseOptions =
        typeCommand
    <|> (   Default
        <$> optional parseSchema
        <*> parseConversion
        <*> optional parseFile
        <*> optional parseOutput
        <*> parseASCII
        <*> parsePlain
        )
    <|> parseVersion
  where
    typeCommand =
        Options.hsubparser
            (Options.command "type" info <> Options.metavar "type")
      where
        info = Options.info parser (Options.progDesc "Output the inferred Dhall type from a YAML value")
        parser =
                Type
            <$> optional parseFile
            <*> optional parseOutput
            <*> parseASCII
            <*> parsePlain

    parseSchema =
        Options.strArgument
            (  Options.metavar "SCHEMA"
            <> Options.help "Dhall type expression (schema)"
            )

    parseVersion =
        Options.flag'
            Version
            (  Options.long "version"
            <> Options.short 'V'
            <> Options.help "Display version"
            )

    parseFile =
        Options.strOption
            (   Options.long "file"
            <>  Options.help "Read YAML expression from a file instead of standard input"
            <>  Options.metavar "FILE"
            <>  Options.action "file"
            )

    parseOutput =
        Options.strOption
            (   Options.long "output"
            <>  Options.help "Write Dhall expression to a file instead of standard output"
            <>  Options.metavar "FILE"
            <>  Options.action "file"
            )

    parseASCII =
        Options.switch
            (   Options.long "ascii"
            <>  Options.help "Format code using only ASCII syntax"
            )

    parsePlain =
        Options.switch
            (   Options.long "plain"
            <>  Options.help "Disable syntax highlighting"
            )

-- ----------
-- Main
-- ----------

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    options <- Options.execParser parserInfo

    let toCharacterSet ascii = case ascii of
            True  -> ASCII
            False -> Unicode

    let toBytes file = case file of
            Nothing   -> BSL8.getContents
            Just path -> BSL8.readFile path

    case options of
        Version ->
            putStrLn (showVersion Meta.version)

        Default{..} -> do
            let characterSet = toCharacterSet ascii

            handle $ do
                yaml <- toBytes file

                expression <- dhallFromYaml (Options schema conversion) yaml

                renderExpression characterSet plain output expression

        Type{..} -> do
            let characterSet = toCharacterSet ascii

            handle $ do
                yaml <- toBytes file

                schema <- YamlToDhall.schemaFromYaml yaml

                renderExpression characterSet plain output schema

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        IO.hPutStrLn IO.stderr ""
        IO.hPrint    IO.stderr e
        System.Exit.exitFailure
