{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Package where

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
import           Dhall.Util         (Input (..), Output (..))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Package"
    [ packageInSameDir
    , packageInParentDir
    , packageInSiblingDir
    ]

packageInSameDir :: TestTree
packageInSameDir = testCase "package in same directory" $ do
    let package :: Expr Void Import
        package = RecordLit $ Map.singleton "test" $
            makeRecordField $ Embed Import
                { importHashed = ImportHashed
                    { hash = Nothing
                    , importType = Local Here File
                        { directory = Directory []
                        , file = "test.dhall"
                        }
                    }
                , importMode = Code
                }
    expr <- getPackageExpression (OutputFile "/dir/package.dhall") (InputFile "/dir/test.dhall" :| [])
    assertEqual "" package expr

packageInParentDir :: TestTree
packageInParentDir = testCase "package in parent directory" $ do
    let package :: Expr Void Import
        package = RecordLit $ Map.singleton "test" $
            makeRecordField $ Embed Import
                { importHashed = ImportHashed
                    { hash = Nothing
                    , importType = Local Here File
                        { directory = Directory ["dir"]
                        , file = "test.dhall"
                        }
                    }
                , importMode = Code
                }
    expr <- getPackageExpression (OutputFile "/package.dhall") (InputFile "/dir/test.dhall" :| [])
    assertEqual "" package expr

packageInSiblingDir :: TestTree
packageInSiblingDir = testCase "package in sibling directory" $ do
    let package :: Expr Void Import
        package = RecordLit $ Map.singleton "test" $
            makeRecordField $ Embed Import
                { importHashed = ImportHashed
                    { hash = Nothing
                    , importType = Local Parent File
                        { directory = Directory ["other", ".."]
                        , file = "test.dhall"
                        }
                    }
                , importMode = Code
                }
    expr <- getPackageExpression (OutputFile "/some/dir/package.dhall") (InputFile "/other/test.dhall" :| [])
    assertEqual "" package expr
