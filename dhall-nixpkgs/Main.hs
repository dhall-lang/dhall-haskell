{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Applicative (empty, optional, (<|>))
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Network.URI (URI(..), URIAuth(..))
import Nix.Expr.Shorthands ((@@))
import Options.Applicative (Parser, ParserInfo)
import System.Exit (ExitCode(..))
import Text.Megaparsec (Parsec)

import qualified Data.Aeson                            as Aeson
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text.Encoding
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import qualified Network.URI                           as URI
import qualified Nix.Expr.Shorthands                   as Nix
import qualified Nix.Pretty
import qualified Options.Applicative                   as Options
import qualified Text.Megaparsec                       as Megaparsec
import qualified Text.Megaparsec.Char                  as Megaparsec.Char
import qualified Turtle

data Options
    = OptionsGitHub GitHub
    | OptionsDirectory Directory

data GitHub = GitHub
    { uri :: Text
    , name :: Maybe Text
    , rev :: Maybe Text
    , hash :: Maybe Text
    , fetchSubmodules :: Bool
    }

data Directory = Directory { path :: FilePath }

data NixPrefetchGit = NixPrefetchGit
    { url :: Text
    , rev :: Text
    , sha256 :: Text
    , fetchSubmodules :: Bool 
    , deepClone :: Bool
    , leaveDotGit :: Bool
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

parseOptions :: Parser Options
parseOptions =
        subcommand "github"    "Use a GitHub repository" parseGitHub
    <|> subcommand "directory" "Use a local directory"   parseDirectory

parseGitHub :: Parser Options
parseGitHub = do
    uri <- Options.strArgument (Options.metavar "URL")

    name <- do
        optional
            (Options.strOption
                (   Options.long "name"
                <>  Options.help "Name for the generated derivation"
                )
            )

    rev <- do
        optional
            (Options.strOption
                (   Options.long "rev"
                <>  Options.help "Revision to use"
                )
            )

    hash <- do
        optional
            (Options.strOption
                (   Options.long "hash"
                <>  Options.help "Expected hash"
                )
            )

    fetchSubmodules <- Options.switch (Options.long "fetch-submodules")

    return (OptionsGitHub GitHub{..})

parseDirectory :: Parser Options
parseDirectory = do
    path <- Options.strArgument (Options.metavar "DIRECTORY")

    return (OptionsDirectory Directory{..})

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
        OptionsGitHub github -> githubToNixpkgs github

toListWith :: (a -> [ Text ]) -> Maybe a -> [ Text ]
toListWith f (Just x ) = f x
toListWith _  Nothing  = [ ]

githubToNixpkgs :: GitHub -> IO ()
githubToNixpkgs GitHub{..} = do
    URI{ uriAuthority = Just URIAuth{..}, .. } <- do
        case URI.parseAbsoluteURI (Text.unpack uri) of
            Nothing -> Turtle.die "The given repository is not a valid URI"

            Just u  -> return u

    case uriScheme of
        "https:" -> do
            return ()
        _ -> do
            Turtle.die "URI schemes other than HTTPS are not supported"

    case uriRegName of
        "github.com" -> do
            return ()
        _ -> do
            Turtle.die "Domains other than github.com are not supported"

    case uriPort of
        "" -> return ()
        _  -> Turtle.die "Non-default ports are not supported"

    case uriQuery of
        "" -> return ()
        _  -> Turtle.die "Non-empty query strings are not supported"

    case uriFragment of
        "" -> return ()
        _  -> Turtle.die "Non-empty query fragments are not supported"

    let githubBase = uriUserInfo <> uriRegName <> uriPort

    let baseUrl = uriScheme <> githubBase <> uriPath <> uriQuery <> uriFragment

    let parsePath :: Parsec Void String (Text, Text)
        parsePath = do
            _ <- Megaparsec.Char.char '/'

            owner <- Megaparsec.takeWhile1P Nothing (/= '/')

            _ <- Megaparsec.Char.char '/'

            repo <- Megaparsec.takeWhile1P Nothing (/= '.')

            optional (Megaparsec.Char.string ".git")

            return (Text.pack owner, Text.pack repo)

    (owner, repo) <- case Megaparsec.parseMaybe parsePath uriPath of
        Nothing -> do
            Turtle.die "The given URL is not a valid GitHub repository"
        Just (owner, repo) -> do
            return (owner, repo)

    let finalName =
            case name of
                Just n  -> n
                Nothing -> repo

    (rev, sha256) <- case rev of
        Just r | not fetchSubmodules -> do
            -- TODO
            return (r, undefined)
        _ -> do
            -- TODO: Don't use the default error handling
            (exitCode, text) <- do
                Turtle.procStrict
                    "nix-prefetch-git"
                    (   [ "--url", Text.pack (baseUrl <> ".git")
                        , "--fetch-submodules"
                        , "--quiet"
                        ]
                    <>  toListWith (\x -> [ "--rev", x ]) rev
                    <>  toListWith (\x -> [ "--hash", x ]) hash
                    )
                    empty

            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> do
                    -- TODO: Include the nix-prefetch-git invocation here
                    Turtle.die "Failed to fetch the GitHub repository"

            let bytes = Text.Encoding.encodeUtf8 text

            NixPrefetchGit{..} <- case Aeson.eitherDecodeStrict' bytes of
                -- TODO: Better error message
                Left _ -> do
                    Turtle.die "Failed to parse the output of nix-prefetch-git"
                Right n -> do
                    return n

            return (rev, sha256)

    let buildDhallGitHubPackage = "buildDhallGitHubPackage"

    let nixExpression =
            Nix.mkFunction
                (Nix.mkParamset
                    [ (buildDhallGitHubPackage, Nothing) ]
                    False
                )
                (   Nix.mkSym buildDhallGitHubPackage
                @@  Nix.attrsE
                        [ ("name", Nix.mkStr finalName)

                        , ("owner", Nix.mkStr owner)
                        , ("repo", Nix.mkStr repo)
                        , ("rev", Nix.mkStr rev)
                        , ("fetchSubmodules", Nix.mkBool fetchSubmodules)
                        -- TODO: Support `private` / `varBase` options
                        , ("githubBase", Nix.mkStr (Text.pack githubBase))
                        , ("sha256", Nix.mkStr sha256)
                        ]
                )

    Prettyprint.Text.putDoc (Nix.Pretty.prettyNix nixExpression)
