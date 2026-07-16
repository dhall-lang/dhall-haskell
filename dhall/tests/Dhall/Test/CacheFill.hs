{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.CacheFill where

import Data.Foldable                    (traverse_)
import Control.Monad                    (void, when)
import System.FilePath                  ((</>))
import Test.Tasty                       (TestTree)
import Test.Tasty.HUnit                 (assertBool, testCase)

import qualified Dhall
import qualified Dhall.Core             as Core
import qualified Dhall.Import           as Import
import qualified System.Directory       as Directory
import qualified System.Environment     as Environment
import qualified System.IO.Temp         as Temp
import qualified Data.Text              as Text

cacheFillTest :: IO ()
cacheFillTest = Temp.withSystemTempDirectory "dhall-cache" $ \cacheDir -> do
    Environment.setEnv "XDG_CACHE_HOME" cacheDir

    let simpleValue = "True"

    expr <- Dhall.inputExpr simpleValue

    let expectedHash = Import.hashExpression (Core.denote expr)

    let semanticCacheDir = cacheDir </> "dhall"

    let cacheFile = semanticCacheDir </> "1220" <> show expectedHash

    tempFile <- Temp.writeTempFile "." "tmp.dhall" (Text.unpack simpleValue)

    let importPath = "./" <> Text.pack tempFile

    let alwaysFailing = "missing"

    let missingWithHash = "missing " <> Import.hashExpressionToCode (Core.denote expr)

    let cases =
            [ "(" <> missingWithHash <> " ? " <> alwaysFailing <> ") ? " <> importPath
            , missingWithHash <> " ? (" <> alwaysFailing <> " ? " <> importPath <> ")"
            , "(" <> alwaysFailing <> " ? " <> missingWithHash <> ") ? " <> importPath
            , alwaysFailing <> " ? (" <> missingWithHash <> " ? " <> importPath <> ")"
            ]

    traverse_ (\exprText -> do
        clearCache semanticCacheDir

        void (Dhall.inputExpr exprText)

        cached <- Directory.doesFileExist cacheFile

        assertBool ("The semantic cache entry for " <> Text.unpack exprText <> " was not written") cached)
            cases

    Directory.removeFile tempFile

clearCache :: FilePath -> IO ()
clearCache cacheDir = do
    exists <- Directory.doesDirectoryExist cacheDir
    when exists (Directory.removeDirectoryRecursive cacheDir)

getTests :: IO TestTree
getTests = return (testCase "Cache write for fallbacks" cacheFillTest)
