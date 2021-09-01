{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (optional, (<|>))
import Control.Exception   (SomeException, throwIO)
import Data.Text           (Text)
import Data.Version        (showVersion)
import Dhall.JSONToDhall
import Dhall.Pretty        (CharacterSet (..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Dhall.Core
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified Options.Applicative        as Options
import qualified Paths_dhall_json           as Meta
import qualified System.Exit
import qualified System.IO                  as IO

-- ---------------
-- Command options
-- ---------------

-- | Command info and description
parserInfo :: ParserInfo Options
parserInfo = Options.info
          (  Options.helper <*> parseOptions)
          (  Options.fullDesc
          <> Options.progDesc "Convert a JSON expression to a Dhall expression, given the expected Dhall type"
          )

-- | All the command arguments and options
data Options
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
    deriving Show

-- | Parser for all the command arguments and options
parseOptions :: Parser Options
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
        info =
            Options.info parser (Options.progDesc "Output the inferred Dhall type from a JSON value")

        parser =
                Type
            <$> optional parseFile
            <*> optional parseOutput
            <*> parseASCII
            <*> parsePlain

    parseSchema =
        Options.strArgument
            (  Options.metavar "SCHEMA"
            <> Options.help "Dhall type (schema).  You can omit the schema to let the executable infer the schema from the JSON value."
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
            <>  Options.help "Read JSON from a file instead of standard input"
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

    let toValue file = do
            bytes <- case file of
                Nothing   -> ByteString.getContents
                Just path -> ByteString.readFile path

            case Aeson.eitherDecode bytes of
                Left err -> throwIO (userError err)
                Right v -> pure v

    let toSchema schema value = do
            finalSchema <- case schema of
                Just text -> resolveSchemaExpr text
                Nothing   -> return (schemaToDhallType (inferSchema value))

            typeCheckSchemaExpr id finalSchema

    case options of
        Version ->
            putStrLn (showVersion Meta.version)

        Default{..} -> do
            let characterSet = toCharacterSet ascii

            handle $ do
                value <- toValue file

                finalSchema <- toSchema schema value

                expression <- Dhall.Core.throws (dhallFromJSON conversion finalSchema value)

                Dhall.Util.renderExpression characterSet plain output expression

        Type{..} -> do
            let characterSet = toCharacterSet ascii

            handle $ do
                value <- toValue file

                finalSchema <- toSchema Nothing value

                Dhall.Util.renderExpression characterSet plain output finalSchema

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        IO.hPutStrLn IO.stderr ""
        IO.hPrint    IO.stderr e
        System.Exit.exitFailure
