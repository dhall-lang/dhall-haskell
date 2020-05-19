{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (optional, (<|>))
import Data.Text (Text)
import Data.Void (Void)
import Network.URI (URI(..), URIAuth(..))
import Nix.Expr.Shorthands ((@@))
import Options.Applicative (Parser, ParserInfo)
import Text.Megaparsec (Parsec)

import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import qualified Network.URI                           as URI
import qualified Nix.Expr.Shorthands                   as Nix
import qualified Nix.Pretty
import qualified Options.Applicative                   as Options
import qualified Text.Megaparsec                       as Megaparsec
import qualified Text.Megaparsec.Char                  as Megaparsec.Char
import qualified Turtle

data Options
    = GitHub
        { uri :: Text
        , name :: Maybe Text
        , revision :: Maybe Text
        , sha256 :: Maybe Text
        , fetchSubmodules :: Bool
        }
    | Directory { path :: FilePath }

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

    revision <- do
        optional
            (Options.strOption
                (   Options.long "revision"
                <>  Options.help "Revision to use"
                )
            )

    sha256 <- do
        optional
            (Options.strOption
                (   Options.long "sha256"
                <>  Options.help "Expected SHA256 hash"
                )
            )

    fetchSubmodules <- Options.switch (Options.long "fetch-submodules")

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

                Just URI{ uriAuthority = Just URIAuth{..}, .. } -> do
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

                    case uriQuery of
                        "" -> do
                            return ()
                        _  -> do
                            Turtle.die "Non-empty query strings are not supported"

                    case uriFragment of
                        "" -> do
                            return ()
                        _  -> do
                            Turtle.die "Non-empty query fragments are not supported"

                    let githubBase =
                            uriUserInfo <> uriRegName <> uriPort

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

                    if fetchSubmodules
                        then do
                            -- TODO: Don't use the default error handling
                            Turtle.procs
                                "nix-prefetch-git"
                                [ ...
                                ]
                                empty

                    let buildDhallGitHubPackage = "buildDhallGitHubPackage"

                    let nixExpression =
                            Nix.mkFunction
                                (Nix.mkParamset
                                    [ (buildDhallGitHubPackage, Nothing) ]
                                    False
                                )
                                (   Nix.mkSym buildDhallGitHubPackage
                                @@  Nix.attrsE
                                        [ ("name"      , Nix.mkStr finalName )

                                        , ("owner"     , Nix.mkStr owner     )
                                        , ("repo"      , Nix.mkStr repo      )
                                        , ("rev"       , Nix.mkStr finalRev  )
                                        , ("githubBase", Nix.mkStr githubBase)
                                        ]
                                )

                    Prettyprint.Text.putDoc (Nix.Pretty.prettyNix nixExpression)
