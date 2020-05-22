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
import Dhall.Import (SemanticCacheMode(..), Status(..))
import Dhall.Parser (Src)
import GHC.Generics (Generic)
import Network.URI (URI(..), URIAuth(..))
import Nix.Expr.Shorthands ((@@), (@.))
import Nix.Expr.Types (NExpr)
import Options.Applicative (Parser, ParserInfo)
import Prelude hiding (FilePath)
import System.Exit (ExitCode(..))
import Text.Megaparsec (Parsec)
import Turtle (FilePath, fp, (</>))

import Dhall.Core
    ( Directory(..)
    , Expr
    , File(..)
    , Import(..)
    , ImportHashed(..)
    , ImportType(..)
    , URL(..)
    )

import qualified Control.Foldl                         as Foldl
import qualified Control.Monad.Trans.State.Strict      as State
import qualified Data.Aeson                            as Aeson
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text.Encoding
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Network.URI                           as URI
import qualified Nix.Expr.Shorthands                   as Nix
import qualified Nix.Pretty
import qualified Options.Applicative                   as Options
import qualified Text.Megaparsec                       as Megaparsec
import qualified Text.Megaparsec.Char                  as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer            as Megaparsec.Char.Lexer
import qualified Turtle

data Options
    = OptionsGitHub GitHub
    | OptionsDirectory Main.Directory

data GitHub = GitHub
    { name :: Maybe Text
    , uri :: Text
    , rev :: Maybe Text
    , hash :: Maybe Text
    , fetchSubmodules :: Bool
    , file :: FilePath
    , source :: Bool
    }

data Directory = Directory
    { name :: Maybe Text
    , directory :: FilePath
    , file :: FilePath
    , source :: Bool
    }

data NixPrefetchGit = NixPrefetchGit
    { url :: Text
    , rev :: Text
    , path :: Text
    , sha256 :: Text
    , fetchSubmodules :: Bool 
    , deepClone :: Bool
    , leaveDotGit :: Bool
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

parseOptions :: Parser Options
parseOptions =
        subcommand
            "github"
            "Use a GitHub repository"
            (fmap OptionsGitHub parseGitHub)
    <|> subcommand
            "directory"
            "Use a local directory"
            (fmap OptionsDirectory parseDirectory)

parseFile :: Parser FilePath
parseFile =
    Options.strOption
        (   Options.long "file"
        <>  Options.help "File to import"
        <>  Options.value "package.dhall"
        )

parseSource :: Parser Bool
parseSource = Options.switch (Options.long "source")

parseName :: Parser (Maybe Text)
parseName =
    optional
        (Options.strOption
            (   Options.long "name"
            <>  Options.help "Name for the Nix derivation"
            )
        )

parseGitHub :: Parser GitHub
parseGitHub = do
    name <- parseName

    uri <- Options.strArgument (Options.metavar "URL")

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

    file <- parseFile

    source <- parseSource

    return GitHub{..}

parseDirectory :: Parser Main.Directory
parseDirectory = do
    name <- parseName

    directory <- Options.strArgument (Options.metavar "DIRECTORY")

    file <- parseFile

    source <- parseSource

    return Main.Directory{..}

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
        OptionsGitHub    github    -> githubToNixpkgs    github
        OptionsDirectory directory -> directoryToNixpkgs directory

toListWith :: (a -> [ Text ]) -> Maybe a -> [ Text ]
toListWith f (Just x ) = f x
toListWith _  Nothing  = [ ]

findExternalDependencies :: FilePath -> Expr Src Import -> IO [Import]
findExternalDependencies baseDirectory expression = do
    let directoryString = Turtle.encodeString baseDirectory

    -- Load the expression once to populate the cache
    _ <- Dhall.Import.loadRelativeTo directoryString UseSemanticCache expression

    -- Now load the same expression a second time so that transitive
    -- dependencies of cached imports are not resolved, and therefore won't
    -- be included in the list
    Status{..} <- State.execStateT (Dhall.Import.loadWith expression) (Dhall.Import.emptyStatus directoryString)

    Turtle.reduce Foldl.list $ do
        let imports = fmap Dhall.Import.chainedImport (Dhall.Map.keys _cache)

        import_@Import{..} <- Turtle.select imports

        let ImportHashed{..} = importHashed

        case importType of
            Remote{} -> case hash of
                Nothing -> do
                    Turtle.die "Unprotected remote imports are not supported"
                Just _ -> do
                    return import_
            Local{} -> do
                empty
            Env{} -> do
                Turtle.die "Environment variable imports are not supported"
            Missing -> do
                Turtle.die "Missing imports are not supported"

dependencyToNix :: Import -> IO ((Text, Maybe nExpr), NExpr)
dependencyToNix import_ = do
    let Import{..} = import_

    let ImportHashed{..} = importHashed

    let prelude = "Prelude"

    case importType of
        Remote URL{..} -> do
            case authority of
                "raw.githubusercontent.com" -> do
                    let File{..} = path

                    let Dhall.Core.Directory{..} = directory

                    case reverse (file : components) of
                        "dhall-lang" : "dhall-lang" : _rev : "Prelude" : rest -> do
                            let fileArgument = Text.intercalate "/" rest

                            return
                                (   (prelude, Nothing)
                                ,       (Nix.mkSym prelude @. "override")
                                    @@  Nix.attrsE
                                            [ ("file", Nix.mkStr fileArgument ) ]
                                )
                        _owner : repo : _rev : rest -> do
                            let fileArgument = Text.intercalate "/" rest

                            return
                                (   (repo, Nothing)
                                ,       (Nix.mkSym repo @. "override")
                                    @@  Nix.attrsE
                                            [ ("file", Nix.mkStr fileArgument ) ]
                                )
                        _ -> do
                            Turtle.die "Not a valid GitHub repository URL"
                "prelude.dhall-lang.org" -> do
                    let File{..} = path

                    let Dhall.Core.Directory{..} = directory

                    let component :: Parsec Void Text Integer
                        component = Megaparsec.Char.Lexer.decimal

                    let version :: Parsec Void Text ()
                        version = do
                            _ <- Megaparsec.Char.char 'v'

                            _ <- component

                            _ <- Megaparsec.Char.char '.'

                            _ <- component

                            _ <- Megaparsec.Char.char '.'

                            _ <- component

                            return ()

                    let path =
                            case reverse (file : components) of
                                first : rest
                                    | Just _ <- Megaparsec.parseMaybe version first ->
                                        rest
                                rest ->
                                    rest
                                
                    let fileArgument = Text.intercalate "/" path

                    return
                        (   (prelude, Nothing)
                        ,       (Nix.mkSym prelude @. "override")
                            @@  Nix.attrsE
                                    [ ("file", Nix.mkStr fileArgument) ]
                        )
                _ -> do
                    Turtle.die "Unsupported dependency"

        _ -> do
            Turtle.die "Internal error"

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

    let githubBase = Text.pack (uriUserInfo <> uriRegName <> uriPort)

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

    let baseUrl =
            Text.pack uriScheme <> "//" <> githubBase <> "/" <> owner <> "/" <> repo

    (rev, sha256, directory) <- case rev of
        Just r | not fetchSubmodules -> do
            (exitCode, text) <- do
                Turtle.procStrict
                    "nix-prefetch-url"
                    (   [ "--unpack"
                        , "--type", "sha256"
                        , "--print-path"
                        , baseUrl <> "/archive/" <> r <> ".tar.gz"
                        ]
                    <>  toListWith (\t -> [ t ]) hash
                    )
                    empty

            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> do
                    -- TODO: Include the nix-prefetch-url invocation here
                    Turtle.die "Failed to fetch the repository archive"

            case Text.lines text of
                [ sha256, path ] -> return (r, sha256, Turtle.fromText path)
                _ -> Turtle.die "Failed to parse the nix-prefetch-url output"

        _ -> do
            (exitCode, text) <- do
                Turtle.procStrict
                    "nix-prefetch-git"
                    (   [ "--url", baseUrl <> ".git"
                        , "--quiet"
                        ]
                    <>  toListWith (\t -> [ "--rev", t ]) rev
                    <>  toListWith (\t -> [ "--hash", t ]) hash
                    <>  (if fetchSubmodules then [ "--fetch-submodules" ] else [])
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

            return (rev, sha256, Turtle.fromText path)

    let finalName =
            case name of
                Nothing -> repo
                Just n  -> n

    let expressionFile = directory </> file

    let baseDirectory = Turtle.directory (directory </> file)

    expressionText <- Turtle.readTextFile expressionFile

    expression <- Dhall.Core.throws (Dhall.Parser.exprFromText (Turtle.encodeString baseDirectory) expressionText)

    dependencies <- findExternalDependencies baseDirectory expression

    nixDependencies <- traverse dependencyToNix dependencies

    let buildDhallGitHubPackage = "buildDhallGitHubPackage"

    let nixExpression =
            Nix.mkFunction
                (Nix.mkParamset
                    (   [ (buildDhallGitHubPackage, Nothing) ]
                    <>  fmap fst nixDependencies
                    )
                    False
                )
                (   Nix.mkSym buildDhallGitHubPackage
                @@  Nix.attrsE
                        [ ("name", Nix.mkStr finalName)
                        , ("githubBase", Nix.mkStr githubBase)
                        , ("owner", Nix.mkStr owner)
                        , ("repo", Nix.mkStr repo)
                        , ("rev", Nix.mkStr rev)
                        , ("fetchSubmodules", Nix.mkBool fetchSubmodules)
                        -- TODO: Support `private` / `varBase` options
                        , ("sha256", Nix.mkStr sha256)
                        , ("file", Nix.mkStr (Turtle.format fp file))
                        , ("source", Nix.mkBool source)
                        , ("dependencies", Nix.mkList (fmap snd nixDependencies))
                        ]
                )

    Prettyprint.Text.putDoc (Nix.Pretty.prettyNix nixExpression)

directoryToNixpkgs :: Main.Directory -> IO ()
directoryToNixpkgs Main.Directory{..} = do
    let finalName =
            case name of
                Nothing -> Turtle.format fp (Turtle.dirname directory)
                Just n  -> n

    expressionText <- Turtle.readTextFile (directory </> file)

    expression <- Dhall.Core.throws (Dhall.Parser.exprFromText (Turtle.encodeString directory) expressionText)

    dependencies <- findExternalDependencies directory expression

    nixDependencies <- traverse dependencyToNix dependencies

    let buildDhallDirectoryPackage = "buildDhallDirectoryPackage"

    let nixExpression =
            Nix.mkFunction
                (Nix.mkParamset
                    (   [ (buildDhallDirectoryPackage, Nothing) ]
                    <>  fmap fst nixDependencies
                    )
                    False
                )
                (   Nix.mkSym buildDhallDirectoryPackage
                @@  Nix.attrsE
                        [ ("name", Nix.mkStr finalName)
                        , ("file", Nix.mkStr (Turtle.format fp file))
                        , ("source", Nix.mkBool source)
                        , ("dependencies", Nix.mkList (fmap snd nixDependencies))
                        ]
                )

    Prettyprint.Text.putDoc (Nix.Pretty.prettyNix nixExpression)
