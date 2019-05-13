{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception (SomeException)
import Data.Aeson (Value)
import Data.Monoid ((<>))
import Dhall.JSON (Conversion, SpecialDoubleMode(..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.JSON
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain    :: Bool
    , omission   :: Value -> Value
    , documents  :: Bool
    , quoted     :: Bool
    , conversion :: Conversion
    }

parseOptions :: Parser Options
parseOptions =
        Options
    <$> parseExplain
    <*> Dhall.JSON.parseOmission
    <*> parseDocuments
    <*> parseQuoted
    <*> Dhall.JSON.parseConversion
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in detail"
            )

    parseDocuments =
        Options.Applicative.switch
            (   Options.Applicative.long "documents"
            <>  Options.Applicative.help "If given a Dhall list, output a document for every element"
            )

    parseQuoted =
        Options.Applicative.switch
            (   Options.Applicative.long "quoted"
            <>  Options.Applicative.help "Prevent from generating not quoted scalars"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc "Compile Dhall to YAML"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options {..} <- Options.Applicative.execParser parserInfo

    handle $ do
        let explaining = if explain then Dhall.detailed else id

        stdin <- Data.Text.IO.getContents

        json <- omission <$> explaining (Dhall.JSON.codeToValue conversion UseYAMLEncoding "(stdin)" stdin)

        let yaml = Dhall.JSON.jsonToYaml json documents quoted

        Data.ByteString.putStr yaml

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
