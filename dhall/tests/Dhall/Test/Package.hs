{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Package where

import           Control.Exception  (Exception, displayException, try)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Void          (Void)
import           Dhall.Core
    ( Directory (..)
    , Expr (..)
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportMode (..)
    , ImportType (..)
    , makeRecordField
    )
import qualified Dhall.Map          as Map
import           Dhall.Package
import           Lens.Micro         (set)
import           System.FilePath    ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Package"
    [ packagePackageFile
    , packageCustomPackageFile
    , packageSingleFile
    , packageEmptyDirectory
    , packageSingleDirectory
    , packageNested
    , packageRecursive
    , packageMissingFile
    , packageFilesDifferentDirs
    , packageIncompatibleFiles
    ]

packagePackageFile :: TestTree
packagePackageFile = testCase "package file" $ do
    let path = "./tests/package" </> "package.dhall"

    let package :: Expr Void Import
        package = RecordLit Map.empty

    (output, expr) <- getPackagePathAndContent defaultOptions ("./tests/package/package.dhall" :| [])
    assertEqual "path" path output
    assertEqual "content" package expr

packageCustomPackageFile :: TestTree
packageCustomPackageFile = testCase "custom package file" $ do
    let path = "./tests/package" </> "custom.dhall"

    let package :: Expr Void Import
        package = RecordLit $
            Map.singleton "package" $ makeRecordField $ Embed packageDhall

    let options :: Options
        options = set packageFileName "custom.dhall" defaultOptions

    (output, expr) <- getPackagePathAndContent options ("./tests/package/package.dhall" :| [])
    assertEqual "path" path output
    assertEqual "content" package expr

packageSingleFile :: TestTree
packageSingleFile = testCase "single file" $ do
    let path = "./tests/package/dir" </> "package.dhall"

    let package :: Expr Void Import
        package = RecordLit $
            Map.singleton "test" $ makeRecordField $ Embed testDhall

    (output, expr) <- getPackagePathAndContent defaultOptions ("./tests/package/dir/test.dhall" :| [])
    assertEqual "path" path output
    assertEqual "content" package expr

packageEmptyDirectory :: TestTree
packageEmptyDirectory = testCase "empty directory" $ do
    let path = "./tests/package/empty" </> "package.dhall"

    let package :: Expr Void Import
        package = RecordLit Map.empty

    (output, expr) <- getPackagePathAndContent defaultOptions ("./tests/package/empty" :| [])
    assertEqual "path" path output
    assertEqual "content" package expr

packageSingleDirectory :: TestTree
packageSingleDirectory = testCase "single directory" $ do
    let path = "./tests/package/dir" </> "package.dhall"

    let package :: Expr Void Import
        package = RecordLit $ Map.singleton "test" $
            makeRecordField $ Embed testDhall

    (output, expr) <- getPackagePathAndContent defaultOptions ("./tests/package/dir" :| [])
    assertEqual "path" path output
    assertEqual "content" package expr

packageNested :: TestTree
packageNested = testCase "nested files" $ do
    let path = "./tests/package" </> "package.dhall"

    let package :: Expr Void Import
        package = RecordLit $ Map.fromList
            [ ("dir", makeRecordField $ RecordLit $ Map.fromList
                [ ("test", makeRecordField $ Embed dirTestDhall)
                ]
              )
            , ("other", makeRecordField $ Embed otherPackageDhall)
            , ("test", makeRecordField $ Embed testDhall)
            ]

    (output, expr) <- getPackagePathAndContent defaultOptions
        ( "./tests/package/test.dhall" :|
        [ "./tests/package/dir/test.dhall"
        , "./tests/package/other/package.dhall"
        ])
    assertEqual "path" path output
    assertEqual "content" package expr

packageRecursive :: TestTree
packageRecursive = testCase "recursively create subpackages" $ do
    let path = "./tests/package/dir" </> "package.dhall"

    let package :: Expr Void Import
        package = RecordLit $ Map.fromList
            [ ("subdirectory1", makeRecordField $ Embed subdirectoryPackageDhall1)
            , ("subdirectory2", makeRecordField $ Embed subdirectoryPackageDhall2)
            , ("test", makeRecordField $ Embed testDhall)
            ]

    let options :: Options
        options = set packagingMode RecursiveSubpackages defaultOptions

    (output, expr) <- getPackagePathAndContent options
        ( "./tests/package/dir" :| [] )
    assertEqual "path" path output
    assertEqual "content" package expr

packageMissingFile :: TestTree
packageMissingFile = testCase "missing file" $ do
    let action :: IO (FilePath, Expr Void Import)
        action = getPackagePathAndContent defaultOptions ("./tests/package/missing.dhall" :| [])

    assertThrow action $ \case
        InvalidPath "./tests/package/missing.dhall" -> True
        _ -> False

packageFilesDifferentDirs :: TestTree
packageFilesDifferentDirs = testCase "files from different directories" $ do
    let action :: IO (FilePath, Expr Void Import)
        action = getPackagePathAndContent defaultOptions
            ( "./tests/package/dir/test.dhall" :|
            [ "./tests/package/test/test.dhall"
            ])

    assertThrow action $ \case
        AmbiguousOutputDirectory "./tests/package/dir" "./tests/package/test" -> True
        _ -> False

packageIncompatibleFiles :: TestTree
packageIncompatibleFiles = testCase "files that are incompatible" $ do
    let action :: IO (FilePath, Expr Void Import)
        action = getPackagePathAndContent defaultOptions
            ( "./tests/package/test.dhall" :|
            [ "./tests/package/test/test.dhall"
            ])

    assertThrow action $ \case
        IncompatiblePaths xs -> xs == [ testDhall , testTestDhall ]
        _ -> False

packageDhall :: Import
packageDhall = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here File
            { directory = Directory []
            , file = "package.dhall"
            }
        }
    , importMode = Code
    }

testDhall :: Import
testDhall = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here File
            { directory = Directory []
            , file = "test.dhall"
            }
        }
    , importMode = Code
    }

dirTestDhall :: Import
dirTestDhall = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here $ File
            { directory = Directory {components = ["dir"]}
            , file = "test.dhall"
            }
        }
    , importMode = Code
    }

otherPackageDhall :: Import
otherPackageDhall = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here $ File
            { directory = Directory {components = ["other"]}
            , file = "package.dhall"
            }
        }
    , importMode = Code
    }

subdirectoryPackageDhall1 :: Import
subdirectoryPackageDhall1 = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here $ File
            { directory = Directory {components = ["subdirectory1"]}
            , file = "package.dhall"}
            }
        , importMode = Code
        }

subdirectoryPackageDhall2 :: Import
subdirectoryPackageDhall2 = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here $ File
            { directory = Directory {components = ["subdirectory2"]}
            , file = "package.dhall"}
            }
        , importMode = Code
        }

testTestDhall :: Import
testTestDhall = Import
    { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local Here (File
            { directory = Directory {components = ["test"]}
            , file = "test.dhall"
            })
        }
    , importMode = Code
    }

assertThrow :: (Exception e, Show a) => IO a -> (e -> Bool) -> IO ()
assertThrow k p = do
    result <- try k
    case result of
        Left e | p e -> return ()
        Left e -> assertFailure $ "Predicate did not match: " <> displayException e
        Right result' -> assertFailure $ "Expected exception, but got: " <> show result'
