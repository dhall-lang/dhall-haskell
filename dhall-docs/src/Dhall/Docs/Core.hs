-- | Contains all the functions that generate documentation

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Dhall.Docs.Core (generateDocs) where

import Data.Monoid         ((<>))
import Data.Text           (Text)
import Data.Void           (Void)
import Dhall.Core          (Binding (..), Expr (..), Import, Var (..), denote)
import Dhall.Docs.Embedded
import Dhall.Docs.Html
import Dhall.Docs.Markdown
import Dhall.Docs.Store
import Dhall.Parser        (Header (..), ParseError (..), exprAndHeaderFromText)
import Dhall.Src           (Src)
import Path                (Abs, Dir, File, Path, Rel, (</>))
import Text.Megaparsec     (ParseErrorBundle (..))

import qualified Control.Applicative as Applicative
import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO        as Text.IO
import qualified Lucid
import qualified Path
import qualified Path.IO
import qualified Text.Megaparsec

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Path (absdir, absfile)

-- | Represents a file that can be rendered as documentation
data DhallFile = DhallFile
    { path :: Path Abs File             -- ^ Path of the file
    , expr :: Expr Src Import           -- ^ File contents
    , header :: Header                  -- ^ Parsed `Header` of the file
    , mType :: Maybe (Expr Void Import) -- ^ Type of the parsed expression,
                                        --   extracted from the source code
    }

{-| Fetches a list of all dhall files in a directory along with its `Header`.
    This is not the same as finding all files that ends in @.dhall@,
    but finds all files that successfully parses as a valid dhall file.

    The reason it doesn't guide the search by its extension is because of the
    dhall <https://prelude.dhall-lang.org Prelude>.
    That package doesn't ends any of their files in @.dhall@.
-}
getAllDhallFiles
    :: Path Abs Dir -- ^ Base directory to do the search
    -> IO [DhallFile]
getAllDhallFiles baseDir = do
    files <- filter hasDhallExtension . snd <$> Path.IO.listDirRecur baseDir
    Data.Maybe.catMaybes <$> mapM readDhall files
  where
    hasDhallExtension :: Path Abs File -> Bool
    hasDhallExtension absFile = case Path.splitExtension absFile of
        Nothing -> False
        Just (_, ext) -> ext == ".dhall"

    readDhall :: Path Abs File -> IO (Maybe DhallFile)
    readDhall absFile = do
        let filePath = Path.fromAbsFile absFile
        contents <- Text.IO.readFile filePath
        case exprAndHeaderFromText filePath contents of
            Right (header, expr) ->
                return $ Just DhallFile
                                { path = absFile
                                , expr, header
                                , mType = extractTypeIfInSource (denote expr :: Expr Void Import)
                                }
            Left ParseError{..} -> do
                putStrLn $ showDhallParseError unwrap
                return Nothing

    showDhallParseError :: Text.Megaparsec.ParseErrorBundle Text Void -> String
    showDhallParseError err =
        "\n\ESC[1;33mWarning\ESC[0m: Invalid Input\n\n" <>
        Text.Megaparsec.errorBundlePretty err <>
        "... documentation won't be generated for this file"

    extractTypeIfInSource :: Expr Void Import -> Maybe (Expr Void Import)
    extractTypeIfInSource expr = do
        V name index <- maybeNameInLet expr
        (Binding _ _ _ (Just (_, exprType)) _ _) <-
            getLetBindingWithIndex index $ getLetBindingsWithName name
        return exprType
      where
        -- | For an expression of the form @let x0 = y0 let x1 = y1 ... in e@
        --   where @e@ is a variable, maybeNameInLet returns the variable name.
        maybeNameInLet :: Expr Void Import -> Maybe Var
        maybeNameInLet (Var v@(V _ _)) = Just v
        maybeNameInLet (Let _ e) = maybeNameInLet e
        maybeNameInLet _ = Nothing


        {-| For an expression of the form @let x0 = y0 x1 = y1 ... in e@
            and a variable name @v@, this returns every @xi@ that is equal to
            v in the reverse order of the source code.

            For example, take a file like this:

        >   let x = 1
        >   let y = 2
        >   let z = 3
        >   in x + y + z

            ... this will return the bindings in this order: [z, y, x]

            Only the "global" level of the file is analyzed
        -}

        getLetBindingsWithName :: Text -> [Binding Void Import]
        getLetBindingsWithName name = filter bindName $ reverse $ go expr
          where
            bindName (Binding _ x _ _ _ _) = x == name

            go :: Expr Void Import -> [Binding Void Import]
            go (Let b@Binding{} e) = b : go e
            go _ = []

        getLetBindingWithIndex :: Int -> [Binding Void Import] -> Maybe (Binding Void Import)
        getLetBindingWithIndex i bindings =
            case drop i bindings of
                [] -> Nothing
                binding : _ -> Just binding

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
    :: Path Abs Dir         -- ^ Input package directory.
                            --   Used to remove the prefix from all other dhall
                            --   files in the package
    -> Path Abs Dir         -- ^ Output directory
    -> Text                 -- ^ Package name
    -> DhallFile            -- ^ Parsed header
    -> IO (Path Abs File)   -- ^ Output path file
saveHtml inputAbsDir outputAbsDir packageName DhallFile {path = absFile, ..} = do
    strippedFilePath <- Path.stripProperPrefix inputAbsDir absFile
    htmlOutputFile <- do
        strippedPathWithExt <- addHtmlExt strippedFilePath
        return (outputAbsDir </> strippedPathWithExt)

    let htmlOutputDir = Path.parent htmlOutputFile

    Path.IO.ensureDir htmlOutputDir

    let relativeResourcesPath = resolveRelativePath outputAbsDir htmlOutputDir

    let strippedHeader = stripCommentSyntax header
    headerAsHtml <- case markdownToHtml absFile strippedHeader of
        Left err -> do
            putStrLn $ markdownParseErrorAsWarning err
            return $ Lucid.toHtml strippedHeader
        Right html -> return html

    Lucid.renderToFile (Path.fromAbsFile htmlOutputFile)
        $ dhallFileToHtml
            strippedFilePath
            expr
            headerAsHtml
            DocParams { relativeResourcesPath, packageName }

    return htmlOutputFile
  where
    addHtmlExt :: Path b File -> IO (Path b File)
    addHtmlExt = Path.addExtension ".html"

    markdownParseErrorAsWarning :: MarkdownParseError -> String
    markdownParseErrorAsWarning MarkdownParseError{..} =
        "\n\ESC[1;33mWarning\ESC[0m\n\n" <>
        Text.Megaparsec.errorBundlePretty unwrap <>
        "The original non-markdown text will be pasted in the documentation"

    stripCommentSyntax :: Header -> Text
    stripCommentSyntax (Header h)
        | Just s <- Data.Text.stripPrefix "--" strippedHeader
            = Data.Text.strip s
        | Just commentPrefixStripped <- Data.Text.stripPrefix "{-" strippedHeader
        , Just commentSuffixStripped <- Data.Text.stripSuffix "-}" commentPrefixStripped
            = Data.Text.strip commentSuffixStripped
        | otherwise = strippedHeader
      where
        strippedHeader = Data.Text.strip h


{-| Create an index.html file on each folder available in the second argument
    that lists all the contents on that folder.

    For example,

    @
    createIndexes [absdir|/|]
        [ [absfile|\/a\/b.txt|]
        , [absfile|\/a\/c/b.txt|]
        , [absfile|\/a\/c.txt"|]
        ]
    @

    ... will create two index.html files:

    1. @\/a\/index.html@, that will list the @\/a\/b.txt@ and
    @\/a\/c.txt@ files
    2. @\/a\/c\/index.html@ that will list the @\/a\/c\/b.txt@ file

-}
createIndexes
    :: Path Abs Dir                 -- ^ Directory where index.html file will be saved. Used
                                    --   to link the css resources. It should be a prefix for
                                    --   each @Path Abs File@ on the second argument
    -> [(Path Abs File, DhallFile)] -- ^ Html files generated by the tool
    -> Text                         -- ^ Package name
    -> IO ()
createIndexes outputPath htmlFiles packageName = do
    let toMap file = Map.singleton (Path.parent $ fst file) [file]
    let filesGroupedByDir = Map.unionsWith (<>) $ map toMap htmlFiles

    let listDirRel dir = do
            dirs <- fst <$> Path.IO.listDir dir
            mapM (Path.stripProperPrefix dir) dirs

    let createIndex :: Path Abs Dir -> [(Path Abs File, DhallFile)] -> IO ()
        createIndex index files = do
            indexFile <- Path.fromAbsFile . (index </>) <$> Path.parseRelFile "index.html"
            indexTitle <-
                if outputPath == index then return $(Path.mkRelDir ".")
                else Path.stripProperPrefix outputPath index
            indexList <- Control.Monad.forM files $ \(fileOutputPath, dhallFile) -> do
                path <- Path.filename <$> Path.stripProperPrefix outputPath fileOutputPath
                return (path, mType dhallFile)
            dirList <- listDirRel index

            let relativeResourcesPath = resolveRelativePath outputPath index
            Lucid.renderToFile indexFile $
                indexToHtml
                    indexTitle
                    indexList
                    dirList
                    DocParams { relativeResourcesPath, packageName }

    _ <- Map.traverseWithKey createIndex filesGroupedByDir
    return ()

-- | Generate all of the docs for a package
generateDocs
    :: Path Abs Dir -- ^ Input directory
    -> Path Abs Dir -- ^ Link to be created to the generated documentation
    -> Text         -- ^ Package name, used in some HTML titles
    -> IO ()
generateDocs inputDir outLink packageName = do

    dhallFilesAndHeaders <- getAllDhallFiles inputDir
    if null dhallFilesAndHeaders then
        putStrLn $
            "No documentation was generated because no file with .dhall " <>
            "extension was found"
    else Path.IO.withSystemTempDir "dhall-docs" $ \tempDir -> do
        Path.IO.ensureDir tempDir
        generatedHtmlFiles <-
            flip zip dhallFilesAndHeaders <$>
                mapM (saveHtml inputDir tempDir packageName) dhallFilesAndHeaders
        createIndexes tempDir generatedHtmlFiles packageName

        dataDir <- getDataDir
        Control.Monad.forM_ dataDir $ \(filename, contents) -> do
            let finalPath = Path.fromAbsFile $ tempDir </> filename
            Data.ByteString.writeFile finalPath contents

        Path.IO.ensureDir $ Path.parent outLink

        outputHash <- makeHashForDirectory tempDir
        outDir <- Applicative.liftA2 (</>)
                    getDocsHomeDirectory
                    (Path.parseRelDir
                        $ show outputHash <> "-" <> Data.Text.unpack packageName)

        Path.IO.copyDirRecur tempDir outDir
        Path.IO.createDirLink outDir outLink
