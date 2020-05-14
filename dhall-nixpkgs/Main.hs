{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Network.URI (URI(..))
import Options.Applicative (Parser, ParserInfo)

import qualified Data.Text           as Text
import qualified Network.URI         as URI
import qualified Options.Applicative as Options
import qualified Turtle

data Options
    = GitHub { uri :: Text }
    | Directory { path :: FilePath }

parseOptions :: Parser Options
parseOptions =
        subcommand "github"    "Use a GitHub repository" parseGitHub
    <|> subcommand "directory" "Use a local directory"   parseDirectory

parseGitHub :: Parser Options
parseGitHub = do
    uri <- Options.strArgument (Options.metavar "URL")

    return GitHub{..}

parseDirectory :: Parser Options
parseDirectory = do
    path <- Options.strArgument (Options.metavar "DIRECTORY")

    return Directory{..}

subcommand :: String -> String -> Parser a -> Parser a
subcommand name description parser =
    Options.hsubparser
        (   Options.command name parserInfo
        <>  Options.metavar name
        )
  where
    parserInfo =
        Options.info parser
            (   Options.fullDesc
            <>  Options.progDesc description
            )

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.info
        (Options.helper <*> parseOptions)
        (   Options.progDesc "Convert a Dhall project to a buildable Nix package"
        <>  Options.fullDesc
        )

main :: IO ()
main = do
    options <- Options.execParser parserInfoOptions

    case options of
        GitHub{..} -> do
            case URI.parseAbsoluteURI (Text.unpack uri) of
                Nothing -> do
                    Turtle.die "The given repository is not a valid URI"
                Just URI{..} -> do
                    print uriScheme
                    print uriAuthority
                    print uriPath
                    print uriQuery
                    print uriFragment
