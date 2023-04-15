{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MultiWayIf   #-}

-- | Create a package.dhall from files and directory contents.

module Dhall.Package
    ( writePackage
    , getPackagePathAndContent
    , PackageError(..)
    ) where

import           Control.Exception  (Exception, throwIO)
import           Control.Monad
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Traversable   (for)
import           Dhall.Core
    ( Directory (..)
    , Expr (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportMode (..)
    , ImportType (..)
    , RecordField (..)
    , makeRecordField
    )
import           Dhall.Map          (Map)
import qualified Dhall.Map          as Map
import           Dhall.Pretty       (CharacterSet (..))
import qualified Dhall.Pretty
import           Dhall.Util         (_ERROR, renderExpression)
import           System.Directory
import           System.FilePath

-- | Create a package.dhall from files and directory contents.
-- For a description of how the package file is constructed see
-- 'getPackagePathAndContent'.
writePackage :: CharacterSet -> Maybe String -> NonEmpty FilePath -> IO ()
writePackage characterSet outputFn inputs = do
    (outputPath, expr) <- getPackagePathAndContent outputFn inputs
    renderExpression characterSet True (Just outputPath) expr

-- | Get the path and the Dhall expression for a package file.
--
-- The location of the resulting package file is determined by the first path of the second argument:
--
--   * If it is a directory, it is also the output directory and the package
--     file will be placed there.
--
--   * If it is a file, then the directory that file resides in is the output
--     directory and the package file will be placed there.
--
-- All inputs provided as the second argument must be either in the output
-- directory or below it. They are processed depending on whether
-- the path points to a directory or a file:
--
--   * If the path points to a directory, all files with a @.dhall@ extensions
--     in that directory are included in the package.
--
--   * If the path points to a regular file, it is included in the package
--     unless it is the path of the package file itself.
--
getPackagePathAndContent :: Maybe String -> NonEmpty FilePath -> IO (FilePath, Expr s Import)
getPackagePathAndContent outputFn (path :| paths) = do
    outputDir <- do
        isDirectory <- doesDirectoryExist path
        return $ if isDirectory then path else takeDirectory path
    outputDir' <- addTrailingPathSeparator <$> makeAbsolute (normalise outputDir)

    -- Check if the supplied @dir@ argument points to a filesystem entry below
    -- the output directory and return the path relative to the output directory.
    let checkOutputDir dir = do
            absoluteDir <- addTrailingPathSeparator <$> makeAbsolute (normalise dir)
            let relativeDir = makeRelative outputDir' absoluteDir
            unless (isRelative relativeDir) $
                throwIO $ AmbiguousOutputDirectory outputDir dir
            return relativeDir

    resultMap <- go Map.empty checkOutputDir (path:paths)
    return (outputDir </> outputFn', RecordLit $ Map.sort resultMap)
    where
        go :: Map Text (RecordField s Import) -> (FilePath -> IO FilePath) -> [FilePath] -> IO (Map Text (RecordField s Import))
        go !acc _checkOutputDir [] = return acc
        go !acc checkOutputDir (p:ps) = do
            isDirectory <- doesDirectoryExist p
            isFile <- doesFileExist p
            if | isDirectory -> do
                    void $ checkOutputDir p
                    entries <- listDirectory p
                    let entries' = filter (\entry -> takeExtension entry == ".dhall") entries
                    go acc checkOutputDir (map (p </>) entries' <> ps)
               | isFile -> do
                    dir <- checkOutputDir $ takeDirectory p

                    let p' = normalise $ dir </> takeFileName p

                    let resultMap = if p' == outputFn'
                            then Map.empty
                            else filepathToMap outputFn' p'

                    acc' <- mergeMaps acc resultMap
                    go acc' checkOutputDir ps
                | otherwise -> throwIO $ InvalidPath p

        outputFn' = fromMaybe "package.dhall" outputFn

-- | Construct a nested 'Map' from a 'FilePath'.
-- For example, the filepath @some/file/path.dhall@ will result in something
-- similar to the following:
--
-- fromList
--   [ ("some", fromList
--     [ ("file", fromList
--       [ ("path", ./some/file/path.dhall)
--       ])
--     ])
--   ])
--
-- ... where ./some/file/path.dhall is a Dhall import. If the last component
-- equals the value passed in the @outputFn@ argument we produce a slightly
-- different result. Consider for example the Dhall Prelude: We have some
-- sub-packages there like @List/package.dhall@. If we want to construct the
-- top-level @package.dhall@ we want an entry like
--
-- > List = ./List/package.dhall
--
-- in there and not:
--
-- > List = { package = ./List/package.dhall }
--
filepathToMap :: FilePath -> FilePath -> Map Text (RecordField s Import)
filepathToMap outputFn = go [] . splitDirectories
    where
        go acc [] = go acc ["."]
        go !acc [x] =
                    let import_ = Import
                            { importHashed = ImportHashed
                                { hash = Nothing
                                , importType = Local Here File
                                    { directory = Directory acc
                                    , file = Text.pack x
                                    }
                                }
                            , importMode = Code
                            }
            in Map.singleton (Text.pack $ dropExtension x) $ makeRecordField $ Embed import_
        go !acc [x, y] | y == outputFn =
                    let import_ = Import
                            { importHashed = ImportHashed
                                { hash = Nothing
                                , importType = Local Here File
                                    { directory = Directory (Text.pack x : acc)
                                    , file = Text.pack y
                                    }
                                }
                            , importMode = Code
                            }
            in Map.singleton (Text.pack x) $ makeRecordField $ Embed import_
        go !acc (x:xs) = Map.singleton (Text.pack x) $ makeRecordField $ RecordLit $ go (Text.pack x : acc) xs

-- | Merge two 'Map's constructed with 'filepathToMap'.
-- It will throw an error if the arguments are not compatible with each other, e.g.
-- we cannot merge the following two maps:
--
-- > fromList [ ("file", ./file.dhall) ]
-- > fromList [ ("file", fromList [("nested", ./file/nested.dhall)]) ]
--
mergeMaps :: Map Text (RecordField s Import) -> Map Text (RecordField s Import) -> IO (Map Text (RecordField s Import))
mergeMaps x y = do
    let x' = fmap (:| []) x
        y' = fmap (:| []) y
        z = Map.unionWith (<>) x' y'
    for z $ \case
        v@RecordField{recordFieldValue = Embed{}} :| [] -> return v
        vs | Just rs <- traverse extractRecordLit vs -> makeRecordField . RecordLit . Map.sort <$> foldM mergeMaps Map.empty rs
           | otherwise -> throwIO $ IncompatiblePaths $ foldMap extractEmbeds vs
    where
        extractEmbeds :: RecordField s Import -> [Import]
        extractEmbeds RecordField{recordFieldValue = Embed import_} = [import_]
        extractEmbeds RecordField{recordFieldValue = RecordLit xs} = foldMap extractEmbeds xs
        extractEmbeds _ = mempty

        extractRecordLit :: RecordField s Import -> Maybe (Map Text (RecordField s Import))
        extractRecordLit RecordField{recordFieldValue = RecordLit xs} = Just xs
        extractRecordLit _ = Nothing

-- | Exception thrown when creating a package file.
data PackageError
    = AmbiguousOutputDirectory FilePath FilePath
    | IncompatiblePaths [Import]
    | InvalidPath FilePath

instance Exception PackageError

instance Show PackageError where
    show (AmbiguousOutputDirectory dir1 dir2) =
        _ERROR <> ": ❰dhall package❱ failed because the inputs make it impossible to\n\
        \determine the output directory of the package file. You asked to include files\n\
        \from the following directories in the package:\n\
        \\n" <> dir1 <>
        "\n" <> dir2 <>
        "\n\n\
        \Although those paths might point to the same location they are not lexically the\n\
        \same."

    show (IncompatiblePaths imports) =
        _ERROR <> ": ❰dhall package❱ failed because some inputs are not compatible with\n\
        \each other:\n\
        \\n" <> unlines (map (show . Dhall.Pretty.prettyExpr . Embed) imports)

    show (InvalidPath fp) =
        _ERROR <> ": ❰dhall package❱ failed because the input does not exist or is\n\
        \neither a directory nor a regular file:\n\
        \\n" <> fp
