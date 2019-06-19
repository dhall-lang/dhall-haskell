{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Exception (SomeException)
import Data.Monoid ((<>))
import Dhall.JSON (parseOmission, parseConversion)
import Dhall.Yaml (Options(..), dhallToYaml, parseDocuments, parseQuoted)
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Text.IO
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.Exit
import qualified System.IO

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

    options <- Options.Applicative.execParser parserInfo

    handle $ do

        stdin <- Data.Text.IO.getContents

        Data.ByteString.putStr =<< dhallToYaml options "(stdin)" stdin

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
