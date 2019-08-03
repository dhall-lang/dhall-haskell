{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), optional)
import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Dhall.JSON (Conversion, SpecialDoubleMode(..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Text.IO             as Text.IO
import qualified Dhall
import qualified Dhall.JSON
import qualified GHC.IO.Encoding
import qualified Options.Applicative      as Options
import qualified Paths_dhall_json         as Meta
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain                   :: Bool
    , pretty                    :: Bool
    , omission                  :: Value -> Value
    , version                   :: Bool
    , conversion                :: Conversion
    , approximateSpecialDoubles :: Bool
    , file                      :: Maybe FilePath
    }

parseOptions :: Parser Options
parseOptions =
        Options
    <$> parseExplain
    <*> parsePretty
    <*> Dhall.JSON.parseOmission
    <*> parseVersion
    <*> Dhall.JSON.parseConversion
    <*> parseApproximateSpecialDoubles
    <*> optional parseFile
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
                <>  Options.help "Pretty print generated JSON"
                )

        compactFlag =
            Options.flag'
                False
                (   Options.long "compact"
                <>  Options.help "Render JSON on one line"
                )

        defaultBehavior =
            pure False

    parseVersion =
        Options.switch
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

    Options {..} <- Options.execParser parserInfo

    when version $ do
      putStrLn (showVersion Meta.version)
      System.Exit.exitSuccess

    handle $ do
        let config = Data.Aeson.Encode.Pretty.Config
                       { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Spaces 2
                       , Data.Aeson.Encode.Pretty.confCompare = compare
                       , Data.Aeson.Encode.Pretty.confNumFormat = Data.Aeson.Encode.Pretty.Generic
                       , Data.Aeson.Encode.Pretty.confTrailingNewline = False }
        let encode =
                if pretty
                then Data.Aeson.Encode.Pretty.encodePretty' config
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

        Data.ByteString.Char8.putStrLn $ Data.ByteString.Lazy.toStrict $ encode json

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
