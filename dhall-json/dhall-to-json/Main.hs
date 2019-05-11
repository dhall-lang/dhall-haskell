{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
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
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.JSON
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified Paths_dhall_json as Meta
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain                   :: Bool
    , pretty                    :: Bool
    , omission                  :: Value -> Value
    , version                   :: Bool
    , conversion                :: Conversion
    , approximateSpecialDoubles :: Bool
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
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in detail"
            )

    parsePretty =
        prettyFlag <|> compactFlag <|> defaultBehavior
      where
        prettyFlag =
            Options.Applicative.flag'
                True
                (   Options.Applicative.long "pretty"
                <>  Options.Applicative.help "Pretty print generated JSON"
                )

        compactFlag =
            Options.Applicative.flag'
                False
                (   Options.Applicative.long "compact"
                <>  Options.Applicative.help "Render JSON on one line"
                )

        defaultBehavior =
            pure False

    parseVersion =
        Options.Applicative.switch
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version"
            )

    parseApproximateSpecialDoubles =
        Options.Applicative.switch
            (   Options.Applicative.long "approximate-special-doubles"
            <>  Options.Applicative.help "Use approximate representation for NaN/±Infinity"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc "Compile Dhall to JSON"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options {..} <- Options.Applicative.execParser parserInfo

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

        stdin <- Data.Text.IO.getContents

        json <- omission <$> explaining (Dhall.JSON.codeToValue conversion specialDoubleMode "(stdin)" stdin)

        Data.ByteString.Char8.putStrLn $ Data.ByteString.Lazy.toStrict $ encode json

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
