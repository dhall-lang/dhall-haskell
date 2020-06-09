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
import Dhall.Docs.Embedded
import Dhall.Docs.Html
import Dhall.Parser        (Header, exprAndHeaderFromText)
import Lucid               (renderToFile)
import Options.Applicative (Parser, ParserInfo)
import Prelude             hiding (FilePath)
import Turtle              (FilePath, Shell, fp, (<.>), (</>))

import qualified Control.Foldl       as Foldl
import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text.Encoding
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
       <> Options.Applicative.help "Directory of your dhall package" )
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
-}
getAllDhallFiles
    :: FilePath -- ^ Base directory to do the search
    -> IO [(FilePath, Header)]
getAllDhallFiles baseDir = Turtle.fold shell Foldl.list
  where
    shell :: Shell (FilePath, Header)
    shell = do
        path_ <- Turtle.lstree baseDir
        False <- Turtle.testdir path_

        let pathStr = Text.unpack $ Turtle.format fp path_
        Right contents <- Turtle.liftIO $ Text.Encoding.decodeUtf8'
                            <$> Data.ByteString.readFile pathStr
        -- contents <- Turtle.liftIO $ Text.IO.readFile pathStr

        case exprAndHeaderFromText pathStr contents of
            Right (header, _) -> return (path_, header)
            _ -> Turtle.empty


{-| Calculate the relative path needed to access files on the first argument
    relative from the second argument.

    The second argument needs to be a child of the first, otherwise it will
    loop forever
-}
resolveRelativePath :: FilePath -> FilePath -> FilePath
resolveRelativePath outDir currentDir =
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
    let toMap file = Map.singleton (Turtle.directory file) [file]
    let filesGroupedByDir = Map.unionsWith (<>) $ map toMap htmlFiles

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

    dataDir <- getDataDir
    Control.Monad.forM_ dataDir $ \(filename, contents) ->
        let finalPath = Turtle.encodeString $
                outDir </> Turtle.decodeString filename
        in
        Data.ByteString.writeFile finalPath contents
    -- Turtle.cptree dataDir outDir

-- | Entry point for the @dhall-docs@ executable
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= defaultMain
