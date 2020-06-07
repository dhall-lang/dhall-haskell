{-| This module contains the top level and options parsing of the @dhall-docs@
    executable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Docs
    ( -- * Options
      Options(..)
    , GenerationStrategy(..)
    , parserInfoOptions
    , parseOptions

      -- * Execution
    , main
    , defaultMain
    , getAllDhallFiles
    ) where

import Data.Semigroup      ((<>))
import Dhall.Parser        (Header, exprAndHeaderFromText)
import Options.Applicative (Parser, ParserInfo)
import Prelude             hiding (FilePath)
import Turtle              (FilePath, fp)

import qualified Control.Foldl       as Foldl
import qualified Control.Monad
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text.IO
import qualified Options.Applicative
import qualified Turtle

{-| To specify if the tool should generate a single HTML page with all the
    package information or one for each file in your package
-}
data GenerationStrategy
    = SinglePage
    | MultiPage
    deriving Show

-- | Command line options
data Options = Options
    { packageDir :: FilePath         -- ^ Directory where your package resides
    , strategy :: GenerationStrategy -- ^ Output strategy of the tool
    , outDir :: FilePath             -- ^ Directory where your documentation
                                     --   will be placed
    }
    deriving Show

parseStrategy :: Parser GenerationStrategy
parseStrategy =
    Options.Applicative.flag
        MultiPage
        SinglePage
        (   Options.Applicative.long "single-page"
        <>  Options.Applicative.help
                "Generate a single page HTML documentation. By default, the tool will generate \
                \a multi-page documentation"
        )

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> Options.Applicative.strOption
        ( Options.Applicative.long "input"
       <> Options.Applicative.metavar "INPUT"
       <> Options.Applicative.help "Directory of your dhall package. It should \
                                    \contain only text files" )
    <*> parseStrategy
    <*> Options.Applicative.strOption
        ( Options.Applicative.long "output"
       <> Options.Applicative.metavar "OUTPUT"
       <> Options.Applicative.help "Directory where your docs will be generated"
       <> Options.Applicative.value "docs" )

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    let progDesc = "Generate HTML documentation from a dhall package or file" in
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc progDesc
        )

{-| Fetches a list of all dhall files in a directory. This is not the same
    as finding all files that ends in @.dhall@, but finds all files that
    successfully parses as a valid dhall file.

    The reason it doesn't guide the search by its extension is because of the
    dhall @Prelude@ (<https://github.com/dhall-lang/dhall-lang/tree/master/Prelude>)
    That package doesn't ends any of their files in @.dhall@.

    TODO: Avoid processing images
-}
getAllDhallFiles
    :: FilePath -- ^ Base directory to do the search
    -> IO [(Header, FilePath)]
getAllDhallFiles baseDir = do

    let shell = do
            path_ <- Turtle.lstree baseDir
            isNotDir <- not <$> Turtle.testdir path_
            Control.Monad.guard isNotDir

            let pathStr = Text.unpack $ Turtle.format fp path_
            contents <- Turtle.liftIO $ Text.IO.readFile pathStr

            case exprAndHeaderFromText pathStr contents of
                Right (header, _) -> return (header, path_)
                _ -> Turtle.empty

    Turtle.fold shell Foldl.list

defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    dhallFiles <- getAllDhallFiles packageDir
    mapM_ print dhallFiles
    return ()

-- | Entry point for the @dhall-docs@ executable
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= defaultMain
