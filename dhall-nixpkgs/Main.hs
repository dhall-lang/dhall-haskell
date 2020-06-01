{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Control.Applicative (empty, optional, (<|>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson (FromJSON)
import Data.Fix (Fix)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Import (Status(..), stack)
import Dhall.Parser (Src)
import GHC.Generics (Generic)
import Lens.Family.State.Strict (zoom)
import Network.URI (URI(..), URIAuth(..))
import Nix.Expr.Shorthands ((@@), (@.))
import Nix.Expr.Types (NExpr)
import Options.Applicative (Parser, ParserInfo)
import Prelude hiding (FilePath)
import System.Exit (ExitCode(..))
import Text.Megaparsec (Parsec)
import Turtle (FilePath, Shell, fp, (</>))

import Dhall.Core
    ( Expr(..)
    , File(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    )

import qualified Control.Foldl                         as Foldl
import qualified Control.Monad.Trans.State.Strict      as State
import qualified Data.Aeson                            as Aeson
import qualified Data.Foldable                         as Foldable
import qualified Data.List.NonEmpty                    as NonEmpty
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text.Encoding
import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Optics
import qualified Dhall.Parser
import qualified GHC.IO.Encoding
import qualified NeatInterpolation
import qualified Network.URI                           as URI
import qualified Nix.Expr.Shorthands                   as Nix
import qualified Nix.Pretty
import qualified Options.Applicative                   as Options
import qualified System.Exit
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
    , directory :: FilePath
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

    directory <-
        Options.strOption
            (   Options.long "directory"
            <>  Options.help "Subdirectory containing the Dhall package"
            <>  Options.value ""
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

die :: MonadIO io => Text -> io a
die text = liftIO $ do
    Text.IO.hPutStr System.IO.stderr text

    System.Exit.exitFailure

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

nub :: Ord a => [a] -> [a]
nub = Foldl.fold Foldl.nub

{-| This specialization of `nub` is necessary to work around a type-checking
    loop
-}
nub'
    :: Ord (f (Fix f)) => [ (Text, Maybe (Fix f)) ] -> [ (Text, Maybe (Fix f)) ]
nub' = nub

{-| The following Nix code is required reading for understanding how
    `findExternalDependencies` needs to work:

    <https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/interpreters/dhall/build-dhall-package.nix>

    The Nixpkgs support for Dhall essentially replaces all remote imports with
    cache hits, but doing so implies that all remote imports must be protected
    by an integrity check.

    This function finds all remote imports that are transitive dependencies of
    the given expression, failing if any of them are missing integrity checks.
-}
findExternalDependencies :: Expr Src Import -> StateT Status Shell URL
findExternalDependencies expression = do
    -- This is a best-effort attempt to pick an import alternative if there is
    -- more than one
    let pickAlt (ImportAlt e0 e1)
            -- If only the latter import has an integrity check, then select
            -- that
            | Embed Import{ importHashed = ImportHashed{ hash = Nothing } } <- Dhall.Core.shallowDenote e0
            , Embed Import{ importHashed = ImportHashed{ hash = Just _  } } <- Dhall.Core.shallowDenote e1 =
                Just e1
            -- Otherwise prefer the first import
            | otherwise =
                Just e0
        pickAlt _ =
            Nothing

    let rewrittenExpression =
            Dhall.Optics.rewriteOf Dhall.Core.subExpressions pickAlt expression

    import_ <- lift (Turtle.select (Foldable.toList rewrittenExpression))

    parent :| _ <- zoom stack State.get

    child <- hoist liftIO (Dhall.Import.chainImport parent import_)

    let Import{ importHashed, importMode } = Dhall.Import.chainedImport child

    let ImportHashed{ hash, importType } = importHashed

    case importMode of
        Code     -> return ()
        RawText  -> return ()
        Location -> empty  -- "as Location" imports aren't real dependencies

    case importType of
        Missing ->
            empty

        Env {} ->
            empty

        Remote url ->
            case hash of
                Just _ -> do
                    return url
                Nothing -> do
                    let dependency = Dhall.Core.pretty url

                    die [NeatInterpolation.text|
Error: Dependency missing a semantic integrity check

Dhall's Nixpkgs requires that all of your remote dependencies are protected by
by semantic integrity checks.  This ensures that Nix can replace the remote
imports with cached imports built by Nix instead of the imports being fetched
via HTTP requests using Dhall.

The following dependency is missing a semantic integrity check:

↳ $dependency
|]

        Local filePrefix file -> do
            filepath <- liftIO (Dhall.Import.localToPath filePrefix file)

            expressionText <- liftIO (Text.IO.readFile filepath)

            parsedExpression <- Dhall.Core.throws (Dhall.Parser.exprFromText filepath expressionText)

            zoom stack (State.modify (NonEmpty.cons child))

            findExternalDependencies parsedExpression

data Dependency = Dependency
    { functionParameter :: (Text, Maybe NExpr)
      -- ^ Function parameter used to bring the dependency into scope for the
      --   Nix package.  The @`Maybe` `NExpr`@ is always `Nothing`, but we
      --   include it here for convenience
    , dependencyExpression :: NExpr
      -- ^ The dependency expression to include in the dependency list.  This
      --   will be an expression of the form:
      --
      --   > someDependency.override { file = "./someFile.dhall" }
    }

{-| The Nixpkgs support for Dhall implements two conventions that
    @dhall-to-nixpkgs@ depends on:

    * Packages are named after their repository name
    * You can import a specific file using `packageName.override { file = …; }`

    This function is responsible for converting Dhall imports to package
    names and files that follow this convention.  For example, given a Dhall
    import like:

        https://raw.githubusercontent.com/EarnestResearch/dhall-packages/master/kubernetes/k8s/1.14.dhall

    ... this will create the corresponding Nix dependency of the form:

        dhall-packages.override { file = "kubernetes/k8s/1.14.dhall"; }
-}
dependencyToNix :: URL -> IO Dependency
dependencyToNix url@URL{ authority, path } = do
    let dependency = Dhall.Core.pretty url

    let prelude = "Prelude"

    case authority of
        "raw.githubusercontent.com" -> do
            let File{ directory, file } = path

            let Dhall.Core.Directory{ components } = directory

            case reverse (file : components) of
                -- Special case to recognize a Prelude import and treat it as if
                -- it were an import of prelude.dhall-lang.org
                "dhall-lang" : "dhall-lang" : _rev : "Prelude" : rest -> do
                    let fileArgument = Text.intercalate "/" rest

                    let functionParameter = (prelude, Nothing)

                    let dependencyExpression =
                                (Nix.mkSym prelude @. "override")
                            @@  Nix.attrsE
                                    [ ("file", Nix.mkStr fileArgument ) ]

                    return Dependency{..}

                _owner : repo : _rev : rest -> do
                    let fileArgument = Text.intercalate "/" rest

                    let functionParameter = (repo, Nothing)

                    let dependencyExpression =
                                (Nix.mkSym repo @. "override")
                            @@  Nix.attrsE
                                    [ ("file", Nix.mkStr fileArgument ) ]

                    return Dependency{..}

                _ -> do
                    die [NeatInterpolation.text|
Error: Not a valid GitHub repository URL

Your Dhall package appears to depend on the following import:

↳ $dependency

... which is missing one or more path components that a raw GitHub import would
normally have.  The URL should minimally have the following path components:

↳ https://raw.githubusercontent.com/$${owner}/$${repository}/$${revision}/…
|]
        "prelude.dhall-lang.org" -> do
            let File{ directory, file } = path

            let Dhall.Core.Directory{ components } = directory

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

            let pathComponents =
                    case reverse (file : components) of
                        first : rest
                            -- Ignore the version.  The Nixpkgs support assumes
                            -- a curated set of package versions.
                            | Just _ <- Megaparsec.parseMaybe version first ->
                                rest
                        rest ->
                            rest
                        
            let fileArgument = Text.intercalate "/" pathComponents

            let functionParameter = (prelude, Nothing)

            let dependencyExpression =
                        (Nix.mkSym prelude @. "override")
                    @@  Nix.attrsE

                            [ ("file", Nix.mkStr fileArgument) ]

            return Dependency{..}
        _ -> do
            die [NeatInterpolation.text|
Error: Unsupported domain

This tool currently only translates the following domains into Nix dependencies:

* raw.githubusercontent.com
* prelude.dhall-lang.org

One of the Dhall project's dependencies:

↳ $dependency

... used the following unexpected domain:

↳ $authority

If you would like to support a new domain for Dhall dependencies, please open an
issue here:

↳ https://github.com/dhall-lang/dhall-haskell/issues
|]

githubToNixpkgs :: GitHub -> IO ()
githubToNixpkgs GitHub{ name, uri, rev = maybeRev, hash, fetchSubmodules, directory, file, source } = do
    URI{ uriScheme, uriAuthority = Just URIAuth{ uriUserInfo, uriRegName, uriPort }, uriPath, uriQuery, uriFragment } <- do
        case URI.parseAbsoluteURI (Text.unpack uri) of
            Nothing -> die [NeatInterpolation.text|
Error: The specified repository is not a valid URI

You provided the following argument:

↳ $uri

... which is not a valid URI
|]

            Just u  -> return u

    case uriScheme of
        "https:" -> do
            return ()
        _ -> do
            let uriSchemeText = Text.pack uriScheme

            die [NeatInterpolation.text|
Error: URI schemes other than https are not supported

You specified the following URI:

↳ $uri

... which has the following scheme:

↳ $uriSchemeText

... which is not https
|]

    case uriRegName of
        "github.com" -> do
            return ()
        _ -> do
            let uriRegNameText = Text.pack uriRegName

            die [NeatInterpolation.text|
Error: Domains other than github.com are not supported

You specified the following URI:

↳ $uri

... which has the following domain:

↳ $uriRegNameText

... which is not github.com
|]

    case uriPort of
        "" -> return ()
        _ -> do
            let uriPortText = Text.pack uriPort

            die [NeatInterpolation.text|
Error: Non-default ports are not supported

You specified the following URI:

↳ $uri

... which has the following explicit port specification:

↳ $uriPortText

... which is not permitted by this tool
|]

    case uriQuery of
        "" -> return ()
        _  -> do
            let uriQueryText = Text.pack uriQuery

            die [NeatInterpolation.text|
Error: Non-empty query strings are not supported

You specified the following URI:

↳ $uri

... which has the following query string:

↳ $uriQueryText

... which is not permitted by this tool
|]

    case uriFragment of
        "" -> return ()
        _  -> do
            let uriFragmentText = Text.pack uriFragment

            die [NeatInterpolation.text|
Error: Non-empty query fragments are not supported

You specified the following URI:

↳ $uri

... which has the following query fragment:

↳ $uriFragmentText

... which is not permitted by this tool
|]

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
            die [NeatInterpolation.text|
Error: Not a valid GitHub repository

You specified the following URI:

↳ $uri

... which is not a valid GitHub repository.  A valid repository must match the
following format:

↳ https://github.com/$${owner}/$${repository}[.git]
|]

        Just (owner, repo) -> do
            return (owner, repo)

    let baseUrl =
            Text.pack uriScheme <> "//" <> githubBase <> "/" <> owner <> "/" <> repo

    (rev, sha256, repository) <- case maybeRev of
        Just r | not fetchSubmodules -> do
            let archiveURL = baseUrl <> "/archive/" <> r <> ".tar.gz"

            let args =  [ "--unpack"
                        , "--type", "sha256"
                        , "--print-path"
                        , "--name", repo
                        , archiveURL
                        ]
                    <>  toListWith (\t -> [ t ]) hash

            let argsText = Text.intercalate " " args

            (exitCode, text) <- Turtle.procStrict "nix-prefetch-url" args empty

            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> do
                    die [NeatInterpolation.text|
Error: Failed to fetch the GitHub's repository archive

The following command failed to fetch the following archive for the repository:

    nix-prefetch-url $argsText
|]

            case Text.lines text of
                [ sha256, path ] -> return (r, sha256, Turtle.fromText path)
                _ -> die [NeatInterpolation.text|
Error: Failed to parse the nix-prefetch-url output

The following command:

    nix-prefetch-url $argsText

... should have produced two lines of output:

* First the SHA256 hash of the GitHub project
* Then the /nix/store/… path of the downloaded project

However, the output did not match, possibly indicating an internal error, either
with this tool or with nix-prefetch-url
|]

        _ -> do
            let args =  [ "--url", baseUrl <> ".git"
                        , "--quiet"
                        ]
                    <>  toListWith (\t -> [ "--rev", t ]) maybeRev
                    <>  toListWith (\t -> [ "--hash", t ]) hash
                    <>  (if fetchSubmodules then [ "--fetch-submodules" ] else [])

            let argsText = Text.intercalate " " args

            (exitCode, text) <- Turtle.procStrict "nix-prefetch-git" args empty

            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> do
                    die [NeatInterpolation.text|
Error: Failed to clone the GitHub repository

The following command failed to clone the repository:

    nix-prefetch-git $argsText
|]

            let bytes = Text.Encoding.encodeUtf8 text

            NixPrefetchGit{ rev, sha256, path } <- case Aeson.eitherDecodeStrict' bytes of
                Left message -> do
                    let messageText = Text.pack message

                    die [NeatInterpolation.text|
Error: Failed to parse the output of nix-prefetch-git

The following command:

    nix-prefetch-url $argsText

... should have produced a JSON output matching the following shape:

↳ { url : Text
  , rev : Text
  , path : Text
  , sha256 : Text
  , fetchSubmodules : Bool 
  }

... but JSON decoding failed with the following error:

↳ $messageText
|]
                Right n -> do
                    return n

            return (rev, sha256, Turtle.fromText path)

    let finalName =
            case name of
                Nothing -> repo
                Just n  -> n

    let expressionFile = repository </> directory </> file

    let baseDirectory = Turtle.directory expressionFile

    let baseDirectoryString = Turtle.encodeString baseDirectory

    exists <- Turtle.testfile expressionFile

    if exists
        then return ()
        else do
            let expressionFileText = Turtle.format fp expressionFile

            die [NeatInterpolation.text|
Error: Missing file

The following file does not exist:

↳ $expressionFileText

Perhaps you meant to specify a different file within the project using the
--file option?
|]

    expressionText <- Turtle.readTextFile expressionFile

    expression <- Dhall.Core.throws (Dhall.Parser.exprFromText baseDirectoryString expressionText)

    let status = Dhall.Import.emptyStatus baseDirectoryString

    dependencies <- Turtle.reduce Foldl.nub (State.evalStateT (findExternalDependencies expression) status)

    nixDependencies <- traverse dependencyToNix dependencies

    let buildDhallGitHubPackage = "buildDhallGitHubPackage"

    let nixExpression =
            Nix.mkFunction
                (Nix.mkParamset
                    (   [ (buildDhallGitHubPackage, Nothing) ]
                    <>  nub' (fmap functionParameter nixDependencies)
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
                        , ("directory", Nix.mkStr (Turtle.format fp directory))
                        , ("file", Nix.mkStr (Turtle.format fp file))
                        , ("source", Nix.mkBool source)
                        , ("dependencies", Nix.mkList (nub (fmap dependencyExpression nixDependencies)))
                        ]
                )

    Prettyprint.Text.putDoc (Nix.Pretty.prettyNix nixExpression)

directoryToNixpkgs :: Directory -> IO ()
directoryToNixpkgs Directory{ name, directory, file, source } = do
    let finalName =
            case name of
                Nothing -> Turtle.format fp (Turtle.dirname directory)
                Just n  -> n

    let expressionFile = directory </> file

    exists <- Turtle.testfile expressionFile

    if exists
        then return ()
        else do
            let expressionFileText = Turtle.format fp expressionFile

            die [NeatInterpolation.text|
Error: Missing file

The following file does not exist:

↳ $expressionFileText

Perhaps you meant to specify a different file within the project using the
--file option?
|]

    expressionText <- Turtle.readTextFile expressionFile

    let directoryString = Turtle.encodeString directory

    expression <- Dhall.Core.throws (Dhall.Parser.exprFromText directoryString expressionText)

    let status = Dhall.Import.emptyStatus directoryString

    dependencies <- Turtle.reduce Foldl.nub (State.evalStateT (findExternalDependencies expression) status)

    nixDependencies <- traverse dependencyToNix dependencies

    let buildDhallDirectoryPackage = "buildDhallDirectoryPackage"

    let src | null directoryString = directoryString
            | otherwise            = init directoryString

    let nixExpression =
            Nix.mkFunction
                (Nix.mkParamset
                    (   [ (buildDhallDirectoryPackage, Nothing) ]
                    <>  nub' (fmap functionParameter nixDependencies)
                    )
                    False
                )
                (   Nix.mkSym buildDhallDirectoryPackage
                @@  Nix.attrsE
                        [ ("name", Nix.mkStr finalName)
                        , ("src", Nix.mkPath False src)
                        , ("file", Nix.mkStr (Turtle.format fp file))
                        , ("source", Nix.mkBool source)
                        , ("dependencies", Nix.mkList (nub (fmap dependencyExpression nixDependencies)))
                        ]
                )

    Prettyprint.Text.putDoc (Nix.Pretty.prettyNix nixExpression)
