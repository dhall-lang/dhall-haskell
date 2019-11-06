{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Applicative (optional, (<|>))
import Control.Exception (SomeException)
import Data.Monoid ((<>))
import Dhall.JSON (parsePreservationAndOmission, parseConversion)
import Dhall.Yaml (Options(..), parseDocuments, parseQuoted)
import Dhall.YAML (dhallToYaml)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Text.IO        as Text.IO
import qualified Data.Version
import qualified GHC.IO.Encoding
import qualified Options.Applicative as Options
import qualified Paths_dhall_yaml    as Meta
import qualified System.Exit
import qualified System.IO

parseOptions :: Parser (Maybe Options)
parseOptions =
            Just
        <$> (   Options
            <$> parseExplain
            <*> Dhall.JSON.parsePreservationAndOmission
            <*> parseDocuments
            <*> parseQuoted
            <*> Dhall.JSON.parseConversion
            <*> optional parseFile
            <*> optional parseOutput
            )
    <|> parseVersion
  where
    parseExplain =
        Options.switch
            (   Options.long "explain"
            <>  Options.help "Explain error messages in detail"
            )

    parseFile =
        Options.strOption
            (   Options.long "file"
            <>  Options.help "Read expression from a file instead of standard input"
            <>  Options.metavar "FILE"
            )

    parseVersion =
        Options.flag'
            Nothing
            (   Options.long "version"
            <>  Options.help "Display version"
            )

    parseOutput =
        Options.strOption
            (   Options.long "output"
            <>  Options.help "Write YAML to a file instead of standard output"
            <>  Options.metavar "FILE"
            )

parserInfo :: ParserInfo (Maybe Options)
parserInfo =
    Options.info
        (Options.helper <*> parseOptions)
        (   Options.fullDesc
        <>  Options.progDesc "Compile Dhall to YAML"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    maybeOptions <- Options.execParser parserInfo

    case maybeOptions of
        Nothing -> do
            putStrLn (Data.Version.showVersion Meta.version)

        Just options@(Options {..}) -> do
            handle $ do
                contents <- case file of
                    Nothing   -> Text.IO.getContents
                    Just path -> Text.IO.readFile path

                let write =
                        case output of
                            Nothing -> Data.ByteString.putStr
                            Just file_ -> Data.ByteString.writeFile file_

                write =<< dhallToYaml options file contents

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
