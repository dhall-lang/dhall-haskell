{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Applicative (optional, (<|>))
import Control.Exception   (SomeException)
import Data.Version        (showVersion)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Csv
import qualified Dhall.Csv.Util
import qualified GHC.IO.Encoding
import qualified Options.Applicative as Options
import qualified Paths_dhall_csv     as Meta
import qualified System.Exit
import qualified System.IO

data Options
    = Options
        { explain                   :: Bool
        , file                      :: Maybe FilePath
        , output                    :: Maybe FilePath
        }
    | Version

parseOptions :: Parser Options
parseOptions =
        (   Options
        <$> parseExplain
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

    parseVersion =
        Options.flag'
            Version
            (   Options.long "version"
            <>  Options.help "Display version"
            )

    parseFile =
        Options.strOption
            (   Options.long "file"
            <>  Options.help "Read expression from a file instead of standard input"
            <>  Options.metavar "FILE"
            <>  Options.action "file"
            )

    parseOutput =
        Options.strOption
            (   Options.long "output"
            <>  Options.help "Write CSV to a file instead of standard output"
            <>  Options.metavar "FILE"
            <>  Options.action "file"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.info
        (Options.helper <*> parseOptions)
        (   Options.fullDesc
        <>  Options.progDesc "Compile Dhall to CSV"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    options <- Options.execParser parserInfo

    case options of
        Version ->
            putStrLn (showVersion Meta.version)
        Options {..} ->
            handle $ do
                let explaining = if explain then Dhall.detailed else id

                text <- case file of
                    Nothing   -> Data.Text.IO.getContents
                    Just path -> Data.Text.IO.readFile path

                csv <- explaining $ Dhall.Csv.codeToValue file text

                let write =
                        case output of
                            Nothing   -> Data.Text.IO.putStr
                            Just path -> Data.Text.IO.writeFile path

                write $ Dhall.Csv.Util.encodeCsvDefault csv

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
