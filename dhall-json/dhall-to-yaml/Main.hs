{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception (SomeException)
import Data.Monoid ((<>))
import Dhall.JSON (Conversion)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Data.Vector
import qualified Data.Yaml
import qualified Dhall
import qualified Dhall.JSON
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain    :: Bool
    , omitNull   :: Bool
    , documents  :: Bool
    , conversion :: Conversion
    }

parseOptions :: Parser Options
parseOptions =
        Options
    <$> parseExplain
    <*> parseOmitNull
    <*> parseDocuments
    <*> Dhall.JSON.parseConversion
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in detail"
            )

    parseOmitNull =
        Options.Applicative.switch
            (   Options.Applicative.long "omitNull"
            <>  Options.Applicative.help "Omit record fields that are null"
            )

    parseDocuments =
        Options.Applicative.switch
            (   Options.Applicative.long "documents"
            <>  Options.Applicative.help "If given a Dhall list, output a document for every element"
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

        let omittingNull = if omitNull then Dhall.JSON.omitNull else id

        stdin <- Data.Text.IO.getContents

        json <- omittingNull <$> explaining (Dhall.JSON.codeToValue conversion "(stdin)" stdin)

        let yaml = case (documents, json) of
              (True, Data.Yaml.Array elems)
                -> Data.ByteString.intercalate "\n---\n"
                   $ fmap Data.Yaml.encode
                   $ Data.Vector.toList elems
              _ -> Data.Yaml.encode json

        Data.ByteString.putStr yaml

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
