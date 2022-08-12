module Dhall.Test.DirectoryTree (tests) where

import Control.Monad
import Data.Either (partitionEithers)
import Lens.Family (set)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.DirectoryTree
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.PosixCompat.Files as Files

tests :: TestTree
tests = testGroup "to-directory-tree"
    [ testGroup "fixpointed"
        [ fixpointedEmpty
        , fixpointedSimple
        , fixpointedMetadata
        ]
    ]

fixpointedEmpty :: TestTree
fixpointedEmpty = testCase "empty" $ do
    let outDir = "./tests/to-directory-tree/fixpoint-empty.out"
        path = "./tests/to-directory-tree/fixpoint-empty.dhall"
    entries <- runDirectoryTree False outDir path
    entries @?= [Directory outDir]

fixpointedSimple :: TestTree
fixpointedSimple = testCase "simple" $ do
    let outDir = "./tests/to-directory-tree/fixpoint-simple.out"
        path = "./tests/to-directory-tree/fixpoint-simple.dhall"
    entries <- runDirectoryTree False outDir path
    entries @?=
        [ Directory outDir
        , File $ outDir </> "file"
        , Directory $ outDir </> "directory"
        ]

fixpointedMetadata :: TestTree
fixpointedMetadata = testCase "metadata" $ do
    let outDir = "./tests/to-directory-tree/fixpoint-metadata.out"
        path = "./tests/to-directory-tree/fixpoint-metadata.dhall"
    entries <- runDirectoryTree False outDir path
    entries @?=
        [ Directory outDir
        , File $ outDir </> "file"
        ]
    s <- Files.getFileStatus $ outDir </> "file"
    let mode = Files.fileMode s `Files.intersectFileModes` Files.accessModes
    mode @?= Files.ownerModes

runDirectoryTree :: Bool -> FilePath -> FilePath -> IO [FilesystemEntry]
runDirectoryTree allowSeparators outDir path = do
    doesOutDirExist <- Directory.doesDirectoryExist outDir
    when doesOutDirExist $
        Directory.removeDirectoryRecursive outDir
    Directory.createDirectoryIfMissing True outDir

    text <- Data.Text.IO.readFile path
    let inputSettings
            = set Dhall.rootDirectory (FilePath.takeDirectory path)
            . set Dhall.sourceName path
            $ Dhall.defaultInputSettings
    expr <- Dhall.inputExprWithSettings inputSettings text

    Dhall.DirectoryTree.toDirectoryTree allowSeparators outDir $ Dhall.Core.denote expr

    walkFsTree outDir

data FilesystemEntry
    = Directory FilePath
    | File FilePath
    deriving (Eq, Show)

walkFsTree :: FilePath -> IO [FilesystemEntry]
walkFsTree dir = do
    entries <- Directory.listDirectory dir
    (ds, fs) <- fmap partitionEithers $ forM entries $ \path -> do
        let path' = dir </> path
        isDirectory <- Directory.doesDirectoryExist path'
        return $ if isDirectory then Left path' else Right (File path')
    entries' <- traverse walkFsTree ds
    return $ Directory dir : fs <> concat entries'
