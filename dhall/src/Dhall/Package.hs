{-# LANGUAGE BangPatterns #-}
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
import           Dhall.Core
    ( Directory (..)
    , Expr (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportMode (..)
    , ImportType (..)
    , RecordField
    , makeRecordField
    )
import           Dhall.Map          (Map)
import qualified Dhall.Map          as Map
import           Dhall.Pretty       (CharacterSet (..))
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
-- The inputs provided as the second argument are processed depending on whether
-- the path points to a directory or a file:
--
--   * If the path points to a directory, all files with a @.dhall@ extensions
--     in that directory are included in the package.
--     The package file will be located in that directory.
--
--   * If the path points to a regular file, it is included in the package
--     unless it is the path of the package file itself.
--     All files passed as input must reside in the same directory.
--     The package file will be located in the (shared) parent directory of the
--     files passed as input to this function.
--
getPackagePathAndContent :: Maybe String -> NonEmpty FilePath -> IO (FilePath, Expr s Import)
getPackagePathAndContent outputFn (path :| paths) = do
    outputDir <- do
        isDirectory <- doesDirectoryExist path
        return $ if isDirectory then path else takeDirectory path
    outputDir' <- makeAbsolute $ normalise outputDir

    let checkOutputDir dir = do
            dir' <- makeAbsolute $ normalise dir
            when (dir' /= outputDir') $
                throwIO $ AmbiguousOutputDirectory outputDir dir

    resultMap <- go Map.empty checkOutputDir (path:paths)
    return (outputDir </> outputFn', RecordLit resultMap)
    where
        go :: Map Text (RecordField s Import) -> (FilePath -> IO ()) -> [FilePath] -> IO (Map Text (RecordField s Import))
        go !acc _checkOutputDir [] = return acc
        go !acc checkOutputDir (p:ps) = do
            isDirectory <- doesDirectoryExist p
            isFile <- doesFileExist p
            if | isDirectory -> do
                    checkOutputDir p
                    entries <- listDirectory p
                    let entries' = filter (\entry -> takeExtension entry == ".dhall") entries
                    go acc checkOutputDir (map (p </>) entries' <> ps)
               | isFile -> do
                    checkOutputDir $ takeDirectory p

                    let key = Text.pack $ dropExtension $ takeFileName p

                    let import_ = Import
                            { importHashed = ImportHashed
                                { hash = Nothing
                                , importType = Local Here File
                                    { directory = Directory []
                                    , file = Text.pack (takeFileName p)
                                    }
                                }
                            , importMode = Code
                            }

                    let resultMap = if takeFileName p == outputFn'
                            then Map.empty
                            else Map.singleton key (makeRecordField $ Embed import_)

                    go (resultMap <> acc) checkOutputDir ps
                | otherwise -> throwIO $ InvalidPath p

        outputFn' = fromMaybe "package.dhall" outputFn

-- | Exception thrown when creating a package file.
data PackageError
    = AmbiguousOutputDirectory FilePath FilePath
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

    show (InvalidPath fp) =
        _ERROR <> ": ❰dhall package❱ failed because the input does not exist or is\n\
        \neither a directory nor a regular file:\n\
        \\n" <> fp
