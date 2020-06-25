{-| This module contains the top level and options parsing of the @dhall-docs@
    executable
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Docs
    ( -- * Options
      Options(..)
    , parserInfoOptions
    , parseOptions

      -- * Execution
    , main
    , defaultMain
    ) where

import Control.Applicative ((<|>))
import Data.Monoid         ((<>))
import Data.Version        (showVersion)
import Dhall.Docs.Core
import Options.Applicative (Parser, ParserInfo)
import Path                (Abs, Dir, Path)

import qualified Control.Monad
import qualified Data.Text
import qualified Data.Text.IO        as Text.IO
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified Path
import qualified Path.IO
import qualified Paths_dhall_docs    as Meta
import qualified System.Directory
import qualified System.Exit
import qualified System.IO

-- | Command line options
data Options
    = Options
        { packageDir :: FilePath         -- ^ Directory where your package resides
        , docLink :: FilePath            -- ^ Link to the generated documentation
        , resolvePackageName :: Path Abs Dir -> String
        }
    | Version

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
    (   Options
    <$> Options.Applicative.strOption
        ( Options.Applicative.long "input"
       <> Options.Applicative.metavar "INPUT"
       <> Options.Applicative.help "Directory of your dhall package" )
    <*> Options.Applicative.strOption
        ( Options.Applicative.long "output-link"
       <> Options.Applicative.metavar "OUTPUT-LINK"
       <> Options.Applicative.help
            ( "Path to the link targeting the directory with the generated "
           <> "documentation. The path needs to not exist or to be a symlink, "
           <> "otherwise the tool won't generate any docs at all"
            )
       <> Options.Applicative.value "./docs" )
    <*> parsePackageNameResolver
    ) <|> parseVersion
  where
    parseVersion =
        Options.Applicative.flag'
            Version
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version"
            )

    parsePackageNameResolver :: Parser (Path Abs Dir -> String)
    parsePackageNameResolver = fmap f (Options.Applicative.optional p)
      where
        -- Directories on the `path` modules always ends in "/", so we have
        -- to remove last one with `init`
        f  Nothing = init . Path.fromRelDir . Path.dirname
        f (Just packageName) = const packageName

        p = Options.Applicative.strOption
                (   Options.Applicative.long "package-name"
                <>  Options.Applicative.metavar "PACKAGE-NAME"
                <>  Options.Applicative.help
                            (  "Override for the package name seen on HTML "
                            <> "navbars. By default, it will extract it from "
                            <> "the input"
                            )
                )

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    let progDesc = "Generate HTML documentation from a dhall package or file" in
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc progDesc
        )


-- | Default execution of @dhall-docs@ command
defaultMain :: Options -> IO ()
defaultMain = \case
    Options{..} -> do
        GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
        resolvedPackageDir <- Path.IO.resolveDir' packageDir

        outDirExists <- System.Directory.doesPathExist docLink
        Control.Monad.when outDirExists $ do
            isLink <- System.Directory.pathIsSymbolicLink docLink
            if isLink then System.Directory.removeFile docLink
            else die $ "The specified --output-link (" <> docLink <> ") already "
                    <> "exists and its not a symlink. The --output-link flag "
                    <> "needs to either not exist or to be a symlink"

        resolvedDocLink <- Path.IO.resolveDir' docLink
        let packageName = resolvePackageName resolvedPackageDir
        generateDocs resolvedPackageDir resolvedDocLink packageName
    Version ->
        putStrLn (showVersion Meta.version)

die :: String -> IO a
die e = do
    Text.IO.hPutStrLn System.IO.stderr $ Data.Text.pack e

    System.Exit.exitFailure

-- | Entry point for the @dhall-docs@ executable
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= defaultMain
