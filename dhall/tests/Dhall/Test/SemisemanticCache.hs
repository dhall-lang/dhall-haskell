{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the disk cache used by Code imports without integrity
-- checks (@$XDG_CACHE_HOME/dhall-haskell-v2/@).
--
-- Imports still typecheck in memory. The on-disk entry for a new Code import
-- is tag 3 plus the CBOR encoding of the inferred type. Tag 1 is an older
-- "already type-checked" marker with no type; the loader still accepts it.
module Dhall.Test.SemisemanticCache where

import Control.Exception            (bracket)
import Control.Monad                (void)
import Data.Foldable                (traverse_)
import Data.Void                    (Void)
import Dhall.Src                    (Src)
import System.FilePath              (takeDirectory, takeFileName, (</>))
import Test.Tasty                   (TestTree)
import Test.Tasty.HUnit             (assertBool, assertEqual, testCase)

import qualified Data.ByteString    as ByteString
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text.IO
import qualified Dhall
import qualified Dhall.Core         as Core
import qualified Lens.Micro
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import qualified System.IO.Temp     as Temp
import qualified Test.Tasty         as Tasty

getTests :: IO TestTree
getTests = return
    (Tasty.testGroup "Semisemantic cache"
        [ testCase "Typechecked marker is cached and reused" typecheckedMarkerCachedTest
        , testCase "Large import stores its type only" largeNFMarkerTest
        , testCase "Child change invalidates parent cache" childChangeInvalidatesParentTest
        ])

withTempCache :: (FilePath -> IO a) -> IO a
withTempCache action =
    Temp.withSystemTempDirectory "dhall-semisemantic" $ \cacheDir -> do
        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache = Environment.setEnv "XDG_CACHE_HOME" cacheDir

        let restoreCache =
                maybe
                    (Environment.unsetEnv "XDG_CACHE_HOME")
                    (Environment.setEnv "XDG_CACHE_HOME")
                    originalCache

        bracket setCache (const restoreCache) (\_ -> action cacheDir)

-- Same directory name as 'Import' uses under @$XDG_CACHE_HOME@.
semisemanticCacheDir :: FilePath -> FilePath
semisemanticCacheDir cacheDir = cacheDir </> "dhall-haskell-v2"

listCacheFiles :: FilePath -> IO [FilePath]
listCacheFiles cacheDir = do
    let dir = semisemanticCacheDir cacheDir
    exists <- Directory.doesDirectoryExist dir
    if exists
        then fmap (map (dir </>)) (Directory.listDirectory dir)
        else return []

-- | Load a Dhall file without putting its filesystem path in the expression.
-- Windows absolute paths such as @C:/Users/...@ are not valid Dhall syntax
-- (the parser reads @C:@ as a label plus annotation).
inputFile :: FilePath -> IO (Core.Expr Src Void)
inputFile path = do
    let settings =
            Lens.Micro.set Dhall.rootDirectory (takeDirectory path)
                Dhall.defaultInputSettings
        importExpr = "./" <> Text.pack (takeFileName path)
    Dhall.inputExprWithSettings settings importExpr

assertNormalizedEqual
    :: String -> Core.Expr Src Void -> Core.Expr Src Void -> IO ()
assertNormalizedEqual message expected actual =
    assertEqual
        message
        (Core.normalize (Core.denote expected) :: Core.Expr Void Void)
        (Core.normalize (Core.denote actual) :: Core.Expr Void Void)

-- | Code imports store the inferred type (tag byte 3); normalization is delayed
--   until the whole resolved expression is evaluated.
typecheckedMarkerCachedTest :: IO ()
typecheckedMarkerCachedTest = withTempCache $ \cacheDir -> do
    tempFile <- Temp.writeTempFile "." "small.dhall" "1 + 1"

    void (inputFile tempFile)
    filesAfterFirst <- listCacheFiles cacheDir
    assertBool
        "first load should write a semisemantic cache entry"
        (not (null filesAfterFirst))

    traverse_ checkTypedTag filesAfterFirst

    result1 <- inputFile tempFile
    result2 <- inputFile tempFile
    assertNormalizedEqual "cached reload should match" result1 result2

    Directory.removeFile tempFile
  where
    checkTypedTag file = do
        bytes <- ByteString.readFile file
        assertEqual
            ("Code imports should store the inferred type in " <> file)
            3
            (ByteString.head bytes)
        assertBool
            ("type payload should not be empty in " <> file)
            (ByteString.length bytes > 1)

-- | Large Text payload: store the inferred type (tag byte 3), not the text.
largeNFMarkerTest :: IO ()
largeNFMarkerTest = withTempCache $ \cacheDir -> do
    let big = Text.replicate 70000 "a"
    tempFile <-
        Temp.writeTempFile "." "large.dhall" (Text.unpack ("\"" <> big <> "\""))

    result1 <- inputFile tempFile
    files <- listCacheFiles cacheDir
    assertBool "large import should write a cache entry" (not (null files))

    traverse_
        (\file -> do
            bytes <- ByteString.readFile file
            assertEqual
                ("large import should store its type in " <> file)
                3
                (ByteString.head bytes)
            size <- Directory.getFileSize file
            assertBool "type file must stay tiny" (size < 32)
        )
        files

    result2 <- inputFile tempFile
    assertNormalizedEqual "marker hit should still evaluate" result1 result2

    Directory.removeFile tempFile

-- | Changing a child file must not reuse a stale parent cache entry.
childChangeInvalidatesParentTest :: IO ()
childChangeInvalidatesParentTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-invalidate" $ \dir -> do
        let childPath = dir </> "child.dhall"
        let parentPath = dir </> "parent.dhall"

        Text.IO.writeFile childPath "1"
        Text.IO.writeFile parentPath "./child.dhall"

        result1 <- inputFile parentPath
        expected1 <- Dhall.inputExpr "1"
        assertNormalizedEqual "initial child value" expected1 result1

        Text.IO.writeFile childPath "2"

        result2 <- inputFile parentPath
        expected2 <- Dhall.inputExpr "2"
        assertNormalizedEqual
            "parent must observe child change (merkle miss)"
            expected2
            result2
