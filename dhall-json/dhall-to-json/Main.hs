{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Applicative (optional, (<|>))
import Control.Exception   (SomeException)
import Data.Aeson          (Value)
import Data.Version        (showVersion)
import Dhall.JSON          (Conversion, SpecialDoubleMode (..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Text
import qualified Data.Aeson.Encode.Pretty          as Pretty
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO                      as Text.IO
import qualified Dhall
import qualified Dhall.JSON
import qualified GHC.IO.Encoding
import qualified Options.Applicative               as Options
import qualified Paths_dhall_json                  as Meta
import qualified System.Exit
import qualified System.IO

data Options
    = Options
        { explain                   :: Bool
        , pretty                    :: Bool
        , omission                  :: Value -> Value
        , conversion                :: Conversion
        , approximateSpecialDoubles :: Bool
        , file                      :: Maybe FilePath
        , output                    :: Maybe FilePath
        }
    | Version

parseOptions :: Parser Options
parseOptions =
        (   Options
        <$> parseExplain
        <*> parsePretty
        <*> Dhall.JSON.parsePreservationAndOmission
        <*> Dhall.JSON.parseConversion
        <*> parseApproximateSpecialDoubles
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

    parsePretty =
        prettyFlag <|> compactFlag <|> defaultBehavior
      where
        prettyFlag =
            Options.flag'
                True
                (   Options.long "pretty"
                <>  Options.help "Deprecated, will be removed soon. Pretty print generated JSON"
                )

        compactFlag =
            Options.flag'
                False
                (   Options.long "compact"
                <>  Options.help "Render JSON on one line"
                )

        defaultBehavior =
            pure True

    parseVersion =
        Options.flag'
            Version
            (   Options.long "version"
            <>  Options.help "Display version"
            )

    parseApproximateSpecialDoubles =
        Options.switch
            (   Options.long "approximate-special-doubles"
            <>  Options.help "Use approximate representation for NaN/±Infinity"
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
            <>  Options.help "Write JSON to a file instead of standard output"
            <>  Options.metavar "FILE"
            <>  Options.action "file"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.info
        (Options.helper <*> parseOptions)
        (   Options.fullDesc
        <>  Options.progDesc "Compile Dhall to JSON"
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
                let custom scientific =
                        Data.Aeson.Text.encodeToTextBuilder (Data.Aeson.Number scientific)

                let config = Pretty.Config
                               { Pretty.confIndent = Pretty.Spaces 2
                               , Pretty.confCompare = compare
                               , Pretty.confNumFormat = Pretty.Custom custom
                               , Pretty.confTrailingNewline = False
                               }
                let encode =
                        if pretty
                        then Pretty.encodePretty' config
                        else Data.Aeson.encode

                let explaining = if explain then Dhall.detailed else id

                let specialDoubleMode =
                        if approximateSpecialDoubles
                        then ApproximateWithinJSON
                        else ForbidWithinJSON

                text <- case file of
                    Nothing   -> Text.IO.getContents
                    Just path -> Text.IO.readFile path

                json <- omission <$> explaining (Dhall.JSON.codeToValue conversion specialDoubleMode file text)

                let write =
                        case output of
                            Nothing -> Data.ByteString.Lazy.putStr
                            Just file_ -> Data.ByteString.Lazy.writeFile file_

                write (encode json <> "\n")

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
