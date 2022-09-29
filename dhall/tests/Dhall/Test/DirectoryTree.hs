{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.DirectoryTree (tests) where

import Control.Monad
import Data.Either (partitionEithers)
import Data.Either.Validation
import Dhall.DirectoryTree (Entry(..), Group(..), User(..))
import Lens.Family (set)
import System.FilePath ((</>))
import System.PosixCompat.Types (FileMode)
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
        [ fixpointedType
        , fixpointedEmpty
        , fixpointedSimple
#ifndef mingw32_HOST_OS
        , fixpointedPermissions
#endif
        , fixpointedUserGroup
        ]
    ]

fixpointedType :: TestTree
fixpointedType = testCase "Type is as expected" $ do
    let file = "./tests/to-directory-tree/type.dhall"
    ref <- Dhall.inputExpr file
    expected' <- case Dhall.DirectoryTree.directoryTreeType of
        Failure e -> assertFailure $ show e
        Success expr -> return expr
    assertBool "Type mismatch" $ expected' `Dhall.Core.judgmentallyEqual` ref

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

{-
This test is disabled on Windows for now as it fails:
    expected: 448
    but got: 438
-}
fixpointedPermissions :: TestTree
fixpointedPermissions = testCase "permissions" $ do
    let outDir = "./tests/to-directory-tree/fixpoint-permissions.out"
        path = "./tests/to-directory-tree/fixpoint-permissions.dhall"
    entries <- runDirectoryTree False outDir path
    entries @?=
        [ Directory outDir
        , File $ outDir </> "file"
        ]
    s <- Files.getFileStatus $ outDir </> "file"
    let mode = Files.fileMode s `Files.intersectFileModes` Files.accessModes
    prettyMode mode @?= prettyMode Files.ownerModes
    where
        prettyMode :: FileMode -> String
        prettyMode m =
            [ 'r' | isBitSet Files.ownerExecuteMode m ] <>
            [ 'w' | isBitSet Files.ownerExecuteMode m ] <>
            [ 'x' | isBitSet Files.ownerExecuteMode m ]

        isBitSet :: FileMode -> FileMode -> Bool
        isBitSet mask m = mask `Files.intersectFileModes` m == Files.nullFileMode

fixpointedUserGroup :: TestTree
fixpointedUserGroup = testCase "user and group" $ do
    let file = "./tests/to-directory-tree/fixpoint-usergroup.dhall"
    expr <- Dhall.inputExpr file
    entries <- Dhall.DirectoryTree.decodeDirectoryTree expr
    entries @?=
        [ Dhall.DirectoryTree.FileEntry $ Entry
            { entryName = "ids"
            , entryContent = ""
            , entryUser = Just (UserId 0)
            , entryGroup = Just (GroupId 0)
            , entryMode = Nothing
            }
        , Dhall.DirectoryTree.FileEntry $ Entry
            { entryName = "names"
            , entryContent = ""
            , entryUser = Just (UserName "user")
            , entryGroup = Just (GroupName "group")
            , entryMode = Nothing
            }
        ]

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
