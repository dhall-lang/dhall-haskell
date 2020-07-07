{-| Contains all the functions that generate documentation

    We should always try to do as little work as possible in an `IO` context.
    To do so, just wrap your function in `IO` if you need to do I/O operations,
    and make pure functions receive that IO result as an input
-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Dhall.Docs.Core (generateDocs, generateDocsPure, GeneratedDocs(..)) where

import Control.Monad.Writer.Class (MonadWriter)
import Data.Map.Strict            (Map)
import Data.Monoid                ((<>))
import Data.Text                  (Text)
import Data.Void                  (Void)
import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , Import
    , MultiLet (..)
    , Var (..)
    , denote
    )
import Dhall.Docs.Embedded
import Dhall.Docs.Html
import Dhall.Docs.Markdown
import Dhall.Docs.Store
import Dhall.Parser
    ( Header (..)
    , ParseError (..)
    , exprAndHeaderFromText
    )
import Dhall.Src                  (Src)
import Path                       (Abs, Dir, File, Path, Rel, (</>))
import Text.Megaparsec            (ParseErrorBundle (..))

import qualified Control.Applicative        as Applicative
import qualified Control.Monad
import qualified Control.Monad.Writer.Class as Writer
import qualified Data.ByteString
import qualified Data.Either
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe
import qualified Data.Maybe                 as Maybe
import qualified Data.Text
import qualified Data.Text.IO               as Text.IO
import qualified Data.Text.Lazy             as Text.Lazy
import qualified Dhall.Core
import qualified Lucid
import qualified Path
import qualified Path.IO
import qualified System.FilePath            as FilePath
import qualified Text.Megaparsec

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Path (reldir)

-- | The result of the doc-generator pure component
data GeneratedDocs a = GeneratedDocs [DocsGenWarning] a

instance Functor GeneratedDocs where
    fmap f (GeneratedDocs w a) = GeneratedDocs w (f a)

instance Applicative GeneratedDocs where
    pure = GeneratedDocs []

    GeneratedDocs w f <*> GeneratedDocs w' a = GeneratedDocs (w <> w') (f a)

instance Monad GeneratedDocs where
    GeneratedDocs w a >>= f =
        let GeneratedDocs w' b = f a
            in GeneratedDocs (w <> w') b

instance MonadWriter [DocsGenWarning] GeneratedDocs where
    tell w = GeneratedDocs w ()

    listen (GeneratedDocs w a) = GeneratedDocs w (a, w)
    pass (GeneratedDocs w (a, f)) = GeneratedDocs (f w) a

data DocsGenWarning
    = InvalidDhall (Text.Megaparsec.ParseErrorBundle Text Void)
    | InvalidMarkdown MarkdownParseError

instance Show DocsGenWarning where
    show (InvalidDhall err) =
        "\n\ESC[1;33mWarning\ESC[0m: Invalid Input\n\n" <>
        Text.Megaparsec.errorBundlePretty err <>
        "... documentation won't be generated for this file"

    show (InvalidMarkdown MarkdownParseError{..}) =
        "\n\ESC[1;33mWarning\ESC[0m: Header comment is not markdown\n\n" <>
        Text.Megaparsec.errorBundlePretty unwrap <>
        "The original non-markdown text will be pasted in the documentation"

-- | Represents a Dhall file that can be rendered as documentation.
--   If you'd like to improve or add features to a .dhall documentation page,
--   add that extra information here.
data DhallFile = DhallFile
    { path :: Path Rel File             -- ^ Path of the file
    , expr :: Expr Src Import           -- ^ File contents
    , header :: Header                  -- ^ Parsed `Header` of the file
    , mType :: Maybe (Expr Void Import) -- ^ Type of the parsed expression,
                                        --   extracted from the source code
    , examples :: [Expr Void Import]    -- ^ Examples extracted from assertions
                                        --   in the file
    }

{-| Takes a list of files paths with their contents and returns the list of
    valid `DhallFile`s.

    Returned files contains all the information to be used on `Html ()`
    generation
-}
getAllDhallFiles :: [(Path Rel File, Text)] -> GeneratedDocs [DhallFile]
getAllDhallFiles = emitErrors . map toDhallFile . filter hasDhallExtension
  where
    hasDhallExtension :: (Path Rel File, Text) -> Bool
    hasDhallExtension (absFile, _) = case Path.splitExtension absFile of
        Nothing -> False
        Just (_, ext) -> ext == ".dhall"

    toDhallFile :: (Path Rel File, Text) -> Either DocsGenWarning DhallFile
    toDhallFile (relFile, contents) =
        case exprAndHeaderFromText (Path.fromRelFile relFile) contents of
            Right (header, expr) ->
                let denoted = denote expr :: Expr Void Import in
                Right DhallFile
                    { path = relFile
                    , expr, header
                    , mType = extractTypeIfInSource denoted
                    , examples = examplesFromAssertions denoted
                    }
            Left ParseError{..} ->
                Left $ InvalidDhall unwrap

    emitErrors :: [Either DocsGenWarning DhallFile] -> GeneratedDocs [DhallFile]
    emitErrors errorsOrDhallFiles = do
        let (errors, dhallFiles) = Data.Either.partitionEithers errorsOrDhallFiles
        Writer.tell errors
        return dhallFiles

    bindings :: Expr Void Import -> [Binding Void Import]
    bindings expr = case expr of
        Let b@Binding{} e ->
            let MultiLet bs _ = Dhall.Core.multiLet b e
            in NonEmpty.toList bs
        _ -> []


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
        getLetBindingsWithName name = filter bindName $ reverse $ bindings expr
          where
            bindName (Binding _ x _ _ _ _) = x == name


        getLetBindingWithIndex :: Int -> [Binding Void Import] -> Maybe (Binding Void Import)
        getLetBindingWithIndex i bs =
            case drop i bs of
                [] -> Nothing
                binding : _ -> Just binding

    examplesFromAssertions :: Expr Void Import -> [Expr Void Import]
    examplesFromAssertions expr = Maybe.mapMaybe fromAssertion values
      where
        values :: [Expr Void Import]
        values = map value $ bindings expr

        fromAssertion :: Expr Void Import -> Maybe (Expr Void Import)
        fromAssertion (Assert e) =  Just e
        fromAssertion _ = Nothing

{-| Given a relative path, returns as much @..\/@ misdirections as needed
    to go to @.@

>>> resolveRelativePath [reldir|.|]
""
>>> resolveRelativePath [reldir|a|]
"../"
>>> resolveRelativePath [reldir|a/b/c|]
"../../../"
-}
resolveRelativePath :: Path Rel Dir -> FilePath
resolveRelativePath currentDir =
    case FilePath.dropTrailingPathSeparator $ Path.fromRelDir currentDir of
        "." -> ""
        _ -> "../" <> resolveRelativePath (Path.parent currentDir)

{-| Generates `Text` from the html representation of a `DhallFile`
-}
makeHtml
    :: Text                 -- ^ Package name
    -> DhallFile            -- ^ Parsed header
    -> GeneratedDocs Text
makeHtml packageName DhallFile {..} = do
    let relativeResourcesPath = resolveRelativePath $ Path.parent path
    let strippedHeader = stripCommentSyntax header
    headerAsHtml <-
        case markdownToHtml path strippedHeader of
            Left err -> do
                Writer.tell [InvalidMarkdown err]
                return $ Lucid.toHtml strippedHeader
            Right html -> return html

    let htmlAsText = Text.Lazy.toStrict $ Lucid.renderText $ dhallFileToHtml
            path
            expr
            examples
            headerAsHtml
            DocParams { relativeResourcesPath, packageName }

    return htmlAsText
  where
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
createIndexes :: Text -> [DhallFile] -> [(Path Rel File, Text)]
createIndexes packageName files = map toIndex dirToDirsAndFilesMapAssocs
  where
    -- Files grouped by their directory
    dirToFilesMap :: Map (Path Rel Dir) [DhallFile]
    dirToFilesMap = Map.unionsWith (<>) $ map toMap files
      where
        toMap :: DhallFile -> Map (Path Rel Dir) [DhallFile]
        toMap dhallFile =
            Map.singleton (Path.parent $ path dhallFile) [dhallFile]

    {-  This is used to compute the list of exported packages on each folder.
        We try to compress the folders as much as we can.

        Take the following directory-tree structure:

      > .
      > ├── a
      > │   └── b
      > │       └── c
      > │           └── b.dhall
      > └── a.dhall

        To improve navigation, the index at @./index.html@ should list
        @a/b/c@ and no @index.html@ should be generated inside of `a/` or
        `a/b/`, but yes on `a/b/c/` in the last one there is the @b.dhall@ file
    -}
    dirToDirsMap :: Map (Path Rel Dir) [Path Rel Dir]
    dirToDirsMap = Map.map removeHereDir $ go dirs initialMap
      where
        removeHereDir :: [Path Rel Dir] -> [Path Rel Dir]
        removeHereDir = filter f
          where
            f :: Path Rel Dir -> Bool
            f reldir = Path.parent reldir /= reldir

        dirs :: [Path Rel Dir]
        dirs = Map.keys dirToFilesMap

        initialMap :: Map (Path Rel Dir) [Path Rel Dir]
        initialMap = Map.fromList $ map (,[]) dirs

        go :: [Path Rel Dir] -> Map (Path Rel Dir) [Path Rel Dir] -> Map (Path Rel Dir) [Path Rel Dir]
        go [] m = m
        go (d : ds) dirMap = go ds $ Map.adjust ([d] <>) (key $ Path.parent d) dirMap
          where
            key :: Path Rel Dir -> Path Rel Dir
            key dir = if dir `Map.member` dirMap then dir else key $ Path.parent dir

    dirToDirsAndFilesMapAssocs :: [(Path Rel Dir, ([DhallFile], [Path Rel Dir]))]
    dirToDirsAndFilesMapAssocs = Map.assocs $ Map.mapWithKey f dirToFilesMap
      where
        f :: Path Rel Dir -> [DhallFile] -> ([DhallFile], [Path Rel Dir])
        f dir dhallFiles = case dirToDirsMap Map.!? dir of
            Nothing -> fileAnIssue "dirToDirsAndFilesMapAssocs"
            Just dirs -> (dhallFiles, dirs)

    toIndex :: (Path Rel Dir, ([DhallFile], [Path Rel Dir])) -> (Path Rel File, Text)
    toIndex (indexDir, (dhallFiles, dirs)) =
        (indexDir </> $(Path.mkRelFile "index.html"), Text.Lazy.toStrict $ Lucid.renderText html)
      where
        html = indexToHtml
            indexDir
            (map (\DhallFile{..} -> (stripPrefix $ addHtmlExt path, mType)) dhallFiles)
            (map stripPrefix dirs)
            DocParams { relativeResourcesPath = resolveRelativePath indexDir, packageName }

        stripPrefix :: Path Rel a -> Path Rel a
        stripPrefix relpath =
            if Path.toFilePath relpath == Path.toFilePath indexDir then relpath
            else Data.Maybe.fromMaybe (fileAnIssue "Bug+with+stripPrefix")
                $ Path.stripProperPrefix indexDir relpath

addHtmlExt :: Path Rel File -> Path Rel File
addHtmlExt relFile =
    Data.Maybe.fromMaybe (fileAnIssue "addHtmlExt") $ Path.addExtension ".html" relFile

-- | If you're wondering the GitHub query params for issue creation:
-- https://docs.github.com/en/github/managing-your-work-on-github/about-automation-for-issues-and-pull-requests-with-query-parameters
fileAnIssue :: Text -> a
fileAnIssue titleName =
    error $ "\ESC[1;31mError\ESC[0mDocumentation generator bug\n\n" <>

            "Explanation: This error message means that there is a bug in the " <>
            "Dhall Documentation generator. You didn't did anything wrong, but " <>
            "if you would like to see this problem fixed then you should report " <>
            "the bug at:\n\n" <>

            "https://github.com/dhall-lang/dhall-haskell/issues/new?labels=dhall-docs,bug\n\n" <>

            "explaining your issue and add \"" <> Data.Text.unpack titleName <> "\" as error code " <>
            "so we can find the proper location in the source code where the error happened\n\n" <>

            "Please, also include your package in the issue. It can be in:\n\n" <>
            "* A compressed archive (zip, tar, etc)\n" <>
            "* A git repository, preferably with a commit reference"

{-| Generate all of the docs for a package. This function does all the `IO ()`
    related tasks to call `generateDocsPure`
-}
generateDocs
    :: Path Abs Dir -- ^ Input directory
    -> Path Abs Dir -- ^ Link to be created to the generated documentation
    -> Text         -- ^ Package name, used in some HTML titles
    -> IO ()
generateDocs inputDir outLink packageName = do
    (_, absFiles) <- Path.IO.listDirRecur inputDir
    contents <- mapM (Text.IO.readFile . Path.fromAbsFile) absFiles
    strippedFiles <- mapM (Path.stripProperPrefix inputDir) absFiles
    let GeneratedDocs warnings docs = generateDocsPure packageName $ zip strippedFiles contents
    mapM_ print warnings
    if null docs then
        putStrLn $
            "No documentation was generated because no file with .dhall " <>
            "extension was found"
    else Path.IO.withSystemTempDir "dhall-docs" $ \tempDir -> do
        copyDataDir tempDir
        mapM_ (writeGenFile tempDir) docs

        outputHash <- makeHashForDirectory tempDir
        outDir <- Applicative.liftA2 (</>)
                    getDocsHomeDirectory
                    (Path.parseRelDir
                        $ show outputHash <> "-" <> Data.Text.unpack packageName)

        Path.IO.copyDirRecur tempDir outDir
        Path.IO.createDirLink outDir outLink
  where
    writeGenFile :: Path Abs Dir -> (Path Rel File, Text) -> IO ()
    writeGenFile outDir (relFile, contents) = do
        Path.IO.ensureDir (outDir </> Path.parent relFile)
        Text.IO.writeFile (Path.fromAbsFile $ outDir </> relFile) contents

    copyDataDir :: Path Abs Dir -> IO ()
    copyDataDir outDir = do
        dataDir <- getDataDir
        Control.Monad.forM_ dataDir $ \(filename, contents) -> do
            let finalPath = Path.fromAbsFile $ outDir </> filename
            Data.ByteString.writeFile finalPath contents

{-| Generates all the documentation of dhall package in a pure way i.e.
    without an `IO` context. This let you generate documentation from a list of
    dhall-files without saving them to the filesystem.

    If you want the `IO` version of this function, check `generateDocs`
-}
generateDocsPure
    :: Text                    -- ^ Package name
    -> [(Path Rel File, Text)] -- ^ (Input file, contents)
    -> GeneratedDocs [(Path Rel File, Text)]
generateDocsPure packageName inputFiles = go
  where
    go :: GeneratedDocs [(Path Rel File, Text)]
    go = do
        dhallFiles <- getAllDhallFiles inputFiles
        htmls <- mapM (makeHtml packageName) dhallFiles
        let indexes = createIndexes packageName dhallFiles
        return (zip (map (addHtmlExt . path) dhallFiles) htmls <> indexes)

