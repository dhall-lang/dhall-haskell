{-| This module contains the top level and options parsing of the @dhall-docs@
    executable
-}
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
    , getAllDhallFilesAndHeaders

      -- * Miscelaneous
    , saveHtml
    , createIndexes
    , resolveRelativePath
    ) where

import Data.Monoid         ((<>))
import Dhall.Docs.Embedded
import Dhall.Docs.Html
import Dhall.Parser        (Header, exprAndHeaderFromText)
import Options.Applicative (Parser, ParserInfo)
import Path                (Abs, Dir, File, Path, (</>))

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe
import qualified Data.Text.Encoding  as Text.Encoding
import qualified Lucid
import qualified Options.Applicative
import qualified Path
import qualified Path.IO

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Path (absdir, absfile)

-- | Command line options
data Options = Options
    { packageDir :: FilePath         -- ^ Directory where your package resides
    , outDir :: FilePath             -- ^ Directory where your documentation
                                      --   will be placed
    , packageNameResolver :: Path Abs Dir -> String
    }

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> Options.Applicative.strOption
        ( Options.Applicative.long "input"
       <> Options.Applicative.metavar "INPUT"
       <> Options.Applicative.help "Directory of your dhall package" )
    <*> Options.Applicative.strOption
        ( Options.Applicative.long "output"
       <> Options.Applicative.metavar "OUTPUT"
       <> Options.Applicative.help "Directory where your docs will be generated"
       <> Options.Applicative.value "docs" )
    <*> parsePackageNameResolver

  where
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

{-| Fetches a list of all dhall files in a directory along with its `Header`.
    This is not the same as finding all files that ends in @.dhall@,
    but finds all files that successfully parses as a valid dhall file.

    The reason it doesn't guide the search by its extension is because of the
    dhall <https://prelude.dhall-lang.org Prelude>.
    That package doesn't ends any of their files in @.dhall@.
-}
getAllDhallFilesAndHeaders
    :: Path Abs Dir -- ^ Base directory to do the search
    -> IO [(Path Abs File, Header)]
getAllDhallFilesAndHeaders baseDir = do
    files <- snd <$> Path.IO.listDirRecur baseDir
    Data.Maybe.catMaybes <$> mapM readDhall files
  where
    readDhall :: Path Abs File -> IO (Maybe (Path Abs File, Header))
    readDhall absFile = do
        let filePath = Path.fromAbsFile absFile
        fileContents <- Data.ByteString.readFile filePath
        return $ case Text.Encoding.decodeUtf8' fileContents of
            Left _ -> Nothing
            Right contents ->
                case exprAndHeaderFromText filePath contents of
                    Right (header, _) -> return (absFile, header)
                    _ -> Nothing


{-| Calculate the relative path needed to access files on the first argument
    relative from the second argument.

    The second argument needs to be a child of the first, otherwise it will
    loop forever

    Examples:

>>> resolveRelativePath [absdir|/a/b/c/|] [absdir|/a/b/c/d/e|]
"../../"
>>> resolveRelativePath [absdir|/a/|] [absdir|/a/|]
""

-}
resolveRelativePath :: Path Abs Dir -> Path Abs Dir -> FilePath
resolveRelativePath outDir currentDir =
    if outDir == currentDir then ""
    else "../" <> resolveRelativePath outDir (Path.parent currentDir)

{-| Saves the HTML file from the input package to the output destination
-}
saveHtml
    :: Path Abs Dir             -- ^ Input package directory.
                                --   Used to remove the prefix from all other dhall
                                --   files in the package
    -> Path Abs Dir             -- ^ Output directory
    -> String                   -- ^ Package name
    -> (Path Abs File, Header)  -- ^ (Input file, Parsed header)
    -> IO (Path Abs File)       -- ^ Output path file
saveHtml inputAbsDir outputAbsDir packageName t@(absFile, _) = do
    htmlOutputFile <- do
        strippedPath <- Path.stripProperPrefix inputAbsDir absFile
        strippedPathWithExt <- addHtmlExt strippedPath
        return (outputAbsDir </> strippedPathWithExt)

    let htmlOutputDir = Path.parent htmlOutputFile

    Path.IO.ensureDir htmlOutputDir

    let relativeResourcesPath = resolveRelativePath outputAbsDir htmlOutputDir

    Lucid.renderToFile (Path.fromAbsFile htmlOutputFile)
        $ filePathHeaderToHtml t DocParams {..}

    return htmlOutputFile
  where
    addHtmlExt :: Path b File -> IO (Path b File)
    addHtmlExt = Path.addExtension ".html"

{-| Create an index.html file on each folder available in the second argument
    that lists all the contents on that folder.

    For example,

    @
    createIndexes [absdir|/|]
        [ [absfile|/a/b.txt|]
        , [absfile|/a/c/b.txt|]
        , [absfile|/a/c.txt"|]
        ]
    @

    ... will create two index.html files:

    1. @/a/index.html@, that will list the @/a/b.txt@ and
    @/a/c.txt@ files
    2. @/a/c/index.html@ that will list the @a/c/b.txt@ file

-}
createIndexes
    :: Path Abs Dir    -- ^ Directory where index.html file will be saved. Used
                       --   to link the css resources. It should be a prefix for
                       --   each @Path Abs File@ on the second argument
    -> [Path Abs File] -- ^ Html files generated by the tool
    -> String          -- ^ Package name
    -> IO ()
createIndexes outputPath htmlFiles packageName = do
    let toMap file = Map.singleton (Path.parent file) [file]
    let filesGroupedByDir = Map.unionsWith (<>) $ map toMap htmlFiles

    let listDirRel dir = do
            dirs <- fst <$> Path.IO.listDir dir
            mapM (Path.stripProperPrefix dir) dirs

    let createIndex index files = do
            indexFile <- Path.fromAbsFile . (index </>) <$> Path.parseRelFile "index.html"
            indexTitle <-
                if outputPath == index then return "package"
                else Path.fromRelDir <$> Path.stripProperPrefix outputPath index
            indexList <- Control.Monad.forM files $
                fmap Path.filename . Path.stripProperPrefix outputPath
            dirList <- listDirRel index

            let relativeResourcesPath = resolveRelativePath outputPath index
            Lucid.renderToFile indexFile $
                indexToHtml
                    indexTitle
                    indexList
                    dirList
                    DocParams
                        { relativeResourcesPath = relativeResourcesPath
                        , packageName = packageName}

    _ <- Map.traverseWithKey createIndex filesGroupedByDir
    return ()

-- | Default execution of @dhall-docs@ command
defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    resolvedPackageDir <- Path.IO.resolveDir' packageDir
    resolvedOutDir <- Path.IO.resolveDir' outDir

    let packageName = packageNameResolver resolvedPackageDir

    dhallFilesAndHeaders <- getAllDhallFilesAndHeaders resolvedPackageDir
    generatedHtmlFiles <-
        mapM (saveHtml resolvedPackageDir resolvedOutDir packageName)
            dhallFilesAndHeaders
    createIndexes resolvedOutDir generatedHtmlFiles packageName

    dataDir <- getDataDir
    Control.Monad.forM_ dataDir $ \(filename, contents) -> do
        let finalPath = Path.fromAbsFile $ resolvedOutDir </> filename
        Data.ByteString.writeFile finalPath contents

-- | Entry point for the @dhall-docs@ executable
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= defaultMain
