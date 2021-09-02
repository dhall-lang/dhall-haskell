{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (optional, (<|>))
import Control.Exception   (SomeException)
import Data.Text           (Text)
import Data.Version        (showVersion)
import Dhall.CsvToDhall
import Dhall.Pretty        (CharacterSet (..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.Text.IO        as Text.IO
import qualified Dhall.Core
import qualified Dhall.Csv.Util
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified Options.Applicative as Options
import qualified Paths_dhall_csv     as Meta
import qualified System.Exit
import qualified System.IO           as IO

parserInfo :: ParserInfo Options
parserInfo = Options.info
          (  Options.helper <*> parseOptions)
          (  Options.fullDesc
          <> Options.progDesc "Convert a CSV expression to a Dhall expression, given the expected Dhall type"
          )

data Options
    = Default
        { schema     :: Maybe Text
        , conversion :: Conversion
        , file       :: Maybe FilePath
        , output     :: Maybe FilePath
        , ascii      :: Bool
        , plain      :: Bool
        , noHeader   :: Bool
        }
    | Type
        { file       :: Maybe FilePath
        , output     :: Maybe FilePath
        , ascii      :: Bool
        , plain      :: Bool
        , noHeader   :: Bool
        }
    | Version
    deriving Show

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
        <*> parseNoHeader
        )
    <|> parseVersion
  where
    typeCommand =
        Options.hsubparser
            (Options.command "type" info <> Options.metavar "type")
      where
        info =
            Options.info parser (Options.progDesc "Output the inferred Dhall type from a CSV value (not implemented)")

        parser =
                Type
            <$> optional parseFile
            <*> optional parseOutput
            <*> parseASCII
            <*> parsePlain
            <*> parseNoHeader

    parseSchema =
        Options.strArgument
            (  Options.metavar "SCHEMA"
            <> Options.help "Dhall type (schema).  You can omit the schema to let the executable infer the schema from the CSV value (not implemented)"
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
            <>  Options.help "Read CSV from a file instead of standard input"
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

    parseNoHeader =
        Options.switch
            (   Options.long "no-header"
            <>  Options.help "Convert CSV with no header"
            )


main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    options <- Options.execParser parserInfo

    let toCharacterSet ascii = case ascii of
            True  -> ASCII
            False -> Unicode

    let toCsv hasHeader file = do
            text <- case file of
                    Nothing -> Text.IO.getContents
                    Just path -> Text.IO.readFile path

            case Dhall.Csv.Util.decodeCsvDefault hasHeader text of
                Left err -> fail err
                Right csv -> pure csv

    let toSchema schema = do
            finalSchema <- case schema of
                Just text -> resolveSchemaExpr text
                Nothing   -> fail "Please specify a schema. Type inference has not been implemented"

            typeCheckSchemaExpr id finalSchema

    case options of
        Version ->
            putStrLn (showVersion Meta.version)

        Default{..} -> do
            handle $ do
                let characterSet = toCharacterSet ascii

                csv <- toCsv (not noHeader) file

                finalSchema <- toSchema schema

                expression <- Dhall.Core.throws $ dhallFromCsv conversion finalSchema csv

                Dhall.Util.renderExpression characterSet plain output expression

        Type{} -> do
            putStrLn "type command has not been implemented yet"


handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        IO.hPutStrLn IO.stderr ""
        IO.hPrint    IO.stderr e
        System.Exit.exitFailure
