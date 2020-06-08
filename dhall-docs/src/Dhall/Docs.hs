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

import Data.Monoid         ((<>))
import Debug.Trace         (trace)
import Dhall.Docs.Html
import Dhall.Parser        (Header, exprAndHeaderFromText)
import Lucid               (renderToFile)
import Options.Applicative (Parser, ParserInfo)
import Paths_dhall_docs
import Prelude             hiding (FilePath)
import Turtle              (FilePath, fp, (<.>), (</>))

import qualified Control.Foldl       as Foldl
import qualified Control.Monad
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe
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
    -> IO [(FilePath, Header)]
getAllDhallFiles baseDir = do

    let shell = do
            path_ <- Turtle.lstree baseDir
            isNotDir <- not <$> Turtle.testdir path_
            Control.Monad.guard isNotDir

            let pathStr = Text.unpack $ Turtle.format fp path_
            contents <- Turtle.liftIO $ Text.IO.readFile pathStr

            case exprAndHeaderFromText pathStr contents of
                Right (header, _) -> return (path_, header)
                _ -> Turtle.empty

    Turtle.fold shell Foldl.list

{-| Calculate the relative path needed to access files on the first argument
    relative from the second argument.

    The second argument needs to be a child of the first, otherwise it will
    loop forever
-}
resolveRelativePath :: FilePath -> FilePath -> FilePath
resolveRelativePath outDir currentDir =
    -- trace (Turtle.encodeString outDir <> " " <> Turtle.encodeString currentDir) $
    if outDir == currentDir then ""
    else ".." </> resolveRelativePath (outDir </> "") (Turtle.parent currentDir)

saveHtml :: FilePath -> FilePath -> (FilePath, Header) -> IO FilePath
saveHtml inputPath outputPath t@(filePath, _) = do
    let inputPathText = Turtle.format fp inputPath
    let filePathText = Turtle.format fp (filePath <.> "html")
    let strippedFilename =
            Text.unpack $ Data.Maybe.fromJust
                $ Text.stripPrefix inputPathText filePathText
    let htmlOutputFile =
            outputPath </> Turtle.decodeString strippedFilename

    let htmlOutputDir = Turtle.directory htmlOutputFile

    Turtle.mktree $ Turtle.directory htmlOutputFile

    let relativeResources = resolveRelativePath outputPath htmlOutputDir

    renderToFile (Turtle.encodeString htmlOutputFile)
        $ filePathHeaderToHtml t (relativeResources </> "index.css")
    return htmlOutputFile

createIndexes :: FilePath -> [FilePath] -> IO ()
createIndexes outputPath htmlFiles = do
    let groupByDir accMap file =
            let directory = Turtle.directory file in
            if directory `Map.notMember` accMap then Map.insert directory [file] accMap
            else Map.adjust ((:) file) directory accMap

    let filesGroupedByDir = foldl groupByDir Map.empty htmlFiles

    let createIndex index files =
            let relativeResources = resolveRelativePath outputPath index
            in
            renderToFile (Turtle.encodeString (index </> "index.html")) $
                indexToHtml index files (relativeResources </> "index.css")

    _ <- Map.traverseWithKey createIndex filesGroupedByDir
    return ()

-- | Default execution of @dhall-docs@ command
defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    dhallFiles <- getAllDhallFiles packageDir
    generatedHtmlFiles <- mapM (saveHtml packageDir outDir) dhallFiles
    createIndexes outDir generatedHtmlFiles

    dataDir <- Turtle.directory . Turtle.decodeString <$> getDataFileName "src/Dhall/data/index.css"
    Turtle.view $ Turtle.lstree dataDir
    Turtle.cptree dataDir outDir

-- | Entry point for the @dhall-docs@ executable
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= defaultMain
