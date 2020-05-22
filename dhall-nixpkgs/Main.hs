{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
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
    ( Expr
    , File(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    )

import qualified Control.Foldl                         as Foldl
import qualified Control.Monad                         as Monad
import qualified Control.Monad.Trans.State.Strict      as State
import qualified Data.Aeson                            as Aeson
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text.Encoding
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified GHC.IO.Encoding
import qualified NeatInterpolation
import qualified Network.URI                           as URI
import qualified Nix.Expr.Shorthands                   as Nix
import qualified Nix.Pretty
import qualified Options.Applicative                   as Options
import qualified System.IO
import qualified Text.Megaparsec                       as Megaparsec
import qualified Text.Megaparsec.Char                  as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer            as Megaparsec.Char.Lexer
import qualified Turtle

data Options
    = OptionsGitHub GitHub
    | OptionsDirectory Directory

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
            "Create a Nix package from a GitHub repository"
            (fmap OptionsGitHub parseGitHub)
    <|> subcommand
            "directory"
            "Create a Nix package from a local directory"
            (fmap OptionsDirectory parseDirectory)
  where
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


parseFile :: Parser FilePath
parseFile =
    Options.strOption
        (   Options.long "file"
        <>  Options.help "File to import, relative to the top-level directory"
        <>  Options.value "package.dhall"
        )

parseSource :: Parser Bool
parseSource =
    Options.switch
        (   Options.long "source"
        <>  Options.help "Configure the Nix package to include source code"
        )

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
                <>  Options.help "Git revision to use"
                )
            )

    hash <- do
        optional
            (Options.strOption
                (   Options.long "hash"
                <>  Options.help "Expected SHA256 hash"
                )
            )

    fetchSubmodules <-
        Options.switch
            (   Options.long "fetch-submodules"
            <>  Options.help "Fetch git submodules"
            )

    file <- parseFile

    source <- parseSource

    return GitHub{..}

parseDirectory :: Parser Directory
parseDirectory = do
    name <- parseName

    directory <- Options.strArgument (Options.metavar "DIRECTORY")

    file <- parseFile

    source <- parseSource

    return Directory{..}

parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.info
        (Options.helper <*> parseOptions)
        (   Options.progDesc "Convert a Dhall project to a Nix package"
        <>  Options.fullDesc
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    options <- Options.execParser parserInfoOptions

    case options of
        OptionsGitHub    github    -> githubToNixpkgs    github
        OptionsDirectory directory -> directoryToNixpkgs directory

-- | Convenient utility for generating command-line options
toListWith :: (a -> [ Text ]) -> Maybe a -> [ Text ]
toListWith f (Just x ) = f x
toListWith _  Nothing  = [ ]

{-| The following Nix code is required reading for understanding how
    `findExternalDependencies` needs to work:

    <https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/interpreters/dhall/build-dhall-package.nix>

    The Nixpkgs support for Dhall essentially replaces all remote imports with
    cache hits, but doing so implies that all remote imports must be protected
    by an integrity check.
-}
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

        -- `as Location` imports do not pull in external dependencies
        Monad.guard (importMode /= Location)

        let ImportHashed{..} = importHashed

        case importType of
            Remote{} -> case hash of
                Nothing -> do
                    let dependency = Dhall.Core.pretty import_

                    Turtle.die [NeatInterpolation.text|
Error: Remote imports require integrity checks

The Nixpkgs support for Dhall replaces all remote imports with cache hits in
order to ensure that Dhall packages built with Nix don't need to make any HTTP
requests.  However, in order for this to work you need to ensure that all remote
imports are protected by integrity checks (e.g. sha256:…).

The following dependency is missing an integrity check:

↳ $dependency
|]
                Just _ -> do
                    return import_
            Local{} -> do
                empty
            Env{} -> do
                -- We intentionally perrmit Dhall packages built using Nix to
                -- refer to environment variables
                empty
            Missing -> do
                -- No need to explicitly fail on missing imports.  Import
                -- resolution will fail anyway before we get to this point.
                empty

{-| The Nixpkgs support for Dhall implements two conventions that
    @dhall-to-nixpkgs@ depends on:

    * Packages are named after their repository name
    * You can import a specific file `packageName.override { file = …; }`

    This function is responsible for converting Dhall imports to package
    names and files that follow this convention.  For example, given a Dhall
    import like:

        https://raw.githubusercontent.com/EarnestResearch/dhall-packages/master/kubernetes/k8s/1.14.dhall

    ... this will create the corresponding Nix dependency of the form:

        dhall-packages.override { file = "kubernetes/k8s/1.14.dhall"; }
-}
dependencyToNix :: Import -> IO ((Text, Maybe nExpr), NExpr)
dependencyToNix import_ = do
    let Import{..} = import_

    let dependency = Dhall.Core.pretty import_

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
                            Turtle.die [NeatInterpolation.text|
Error: Not a valid GitHub repository URL

Your Dhall package appears to depend on the following import:

↳ $dependency

... which is missing one or more path components that a raw GitHub import would
normally have.  The URL should minimally have the following path components:

↳ https://raw.githubusercontent.com/$${owner}/$${repository}/$${revision}/…
|]
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
                    Turtle.die [NeatInterpolation.text|
Error: Unsupported domain

This tool currently only translates the following domains into Nix dependencies:

* raw.githubusercontent.com
* prelude.dhall-lang.org

One of the Dhall project's dependencies was for an unexpected domain:

↳ $dependency

If you would like to support a new domain for Dhall dependencies, please open an
issue here:

↳ https://github.com/dhall-lang/dhall-haskell/issues
|]

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

directoryToNixpkgs :: Directory -> IO ()
directoryToNixpkgs Directory{..} = do
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
