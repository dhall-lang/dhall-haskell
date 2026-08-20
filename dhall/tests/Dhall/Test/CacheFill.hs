{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for opportunistic semantic-cache fill.
--
-- This is a dhall-haskell implementation optimization (not part of the
-- language standard): when resolving @p ? q@, if @p@ is a missing
-- hash-protected import and @q@ succeeds with a matching semantic hash, the
-- result is written to the semantic cache under that hash.
module Dhall.Test.CacheFill where

import Control.Exception                (bracket)
import Control.Monad                    (void, when)
import Data.Foldable                    (traverse_)
import Data.Text                        (Text)
import Data.Void                        (Void)
import Dhall.Src                        (Src)
import System.FilePath                  (takeDirectory, takeFileName, (</>))
import Test.Tasty                       (TestTree)
import Test.Tasty.HUnit                 (assertBool, assertEqual, testCase)

import qualified Data.Text              as Text
import qualified Dhall
import qualified Dhall.Core             as Core
import qualified Dhall.Import           as Import
import qualified Lens.Micro
import qualified System.Directory       as Directory
import qualified System.Environment     as Environment
import qualified System.IO.Temp         as Temp
import qualified Test.Tasty             as Tasty

getTests :: IO TestTree
getTests = return
    (Tasty.testGroup "Cache fill tests"
        [ testCase "Cache write for fallbacks" cacheFillAssociativityTest
        , testCase "Opportunistic fill then cache hit" fillThenHitTest
        , testCase "Opportunistic fill from env import" envFillThenHitTest
        , testCase "Hash mismatch does not fill cache" hashMismatchDoesNotFillTest
        , testCase "Hash on successful right branch is ignored" hashOnRightIsIgnoredTest
        , testCase "Opportunistic fill normalizes unhashed Code" fillFromUnnormalizedImportTest
        ])

-- | Isolate each test in a fresh semantic cache directory.
withTempCache :: (FilePath -> IO a) -> IO a
withTempCache action =
    Temp.withSystemTempDirectory "dhall-cache" $ \cacheDir -> do
        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache = Environment.setEnv "XDG_CACHE_HOME" cacheDir

        let restoreCache =
                maybe
                    (Environment.unsetEnv "XDG_CACHE_HOME")
                    (Environment.setEnv "XDG_CACHE_HOME")
                    originalCache

        bracket setCache (const restoreCache) (\_ -> action cacheDir)

semanticCacheFile :: FilePath -> Core.Expr s Void -> FilePath
semanticCacheFile cacheDir expr =
    let hash = Import.hashExpression (Core.denote expr)
    in  cacheDir </> "dhall" </> ("1220" <> show hash)

clearSemanticCache :: FilePath -> IO ()
clearSemanticCache cacheDir = do
    let semanticCacheDir = cacheDir </> "dhall"
    exists <- Directory.doesDirectoryExist semanticCacheDir
    when exists (Directory.removeDirectoryRecursive semanticCacheDir)

writeTempImport :: Text -> IO FilePath
writeTempImport contents =
    Temp.writeTempFile "." "tmp.dhall" (Text.unpack contents)

-- | Relative import of a temp file. Windows absolute paths such as
-- @C:/Users/...@ are not valid Dhall syntax.
relativeImport :: FilePath -> Text
relativeImport path = "./" <> Text.pack (takeFileName path)

-- | Evaluate an expression whose relative imports live next to @path@.
inputExprNear :: FilePath -> Text -> IO (Core.Expr Src Void)
inputExprNear path expr = do
    let settings =
            Lens.Micro.set Dhall.rootDirectory (takeDirectory path)
                Dhall.defaultInputSettings
    Dhall.inputExprWithSettings settings expr

assertNormalizedEqual
    :: String -> Core.Expr Src Void -> Core.Expr Src Void -> IO ()
assertNormalizedEqual message expected actual =
    assertEqual
        message
        (Core.normalize (Core.denote expected) :: Core.Expr Void Void)
        (Core.normalize (Core.denote actual) :: Core.Expr Void Void)

-- | Associativity of @?@: a matching hash anywhere to the left of a successful
-- fallback should opportunistically populate the semantic cache.
cacheFillAssociativityTest :: IO ()
cacheFillAssociativityTest = withTempCache $ \cacheDir -> do
    let simpleValue = "True"

    expr <- Dhall.inputExpr simpleValue

    let cacheFile = semanticCacheFile cacheDir expr

    tempFile <- writeTempImport simpleValue

    let importPath = relativeImport tempFile

    let alwaysFailing = "missing"

    let missingWithHash =
            "missing " <> Import.hashExpressionToCode (Core.denote expr)

    let cases =
            [ "(" <> missingWithHash <> " ? " <> alwaysFailing <> ") ? " <> importPath
            , missingWithHash <> " ? (" <> alwaysFailing <> " ? " <> importPath <> ")"
            , "(" <> alwaysFailing <> " ? " <> missingWithHash <> ") ? " <> importPath
            , alwaysFailing <> " ? (" <> missingWithHash <> " ? " <> importPath <> ")"
            ]

    traverse_
        (\exprText -> do
            clearSemanticCache cacheDir

            void (inputExprNear tempFile exprText)

            cached <- Directory.doesFileExist cacheFile

            assertBool
                ("The semantic cache entry for " <> Text.unpack exprText <> " was not written")
                cached)
        cases

    Directory.removeFile tempFile

-- | After an opportunistic fill, a later @missing sha256:… ? fallback@ reads
-- the cached value instead of evaluating the new fallback.
fillThenHitTest :: IO ()
fillThenHitTest = withTempCache $ \cacheDir -> do
    let cachedValue = "123"

    expr <- Dhall.inputExpr cachedValue

    let hashCode = Import.hashExpressionToCode (Core.denote expr)
    let cacheFile = semanticCacheFile cacheDir expr

    tempFile <- writeTempImport cachedValue
    let importPath = relativeImport tempFile

    step1 <- Dhall.inputExpr ("missing " <> hashCode <> " ? 10")
    assertNormalizedEqual
        "cache miss should use the literal fallback"
        (Core.NaturalLit 10)
        step1

    cachedBeforeFill <- Directory.doesFileExist cacheFile
    assertBool "cache should be empty before opportunistic fill" (not cachedBeforeFill)

    step2 <- inputExprNear tempFile ("missing " <> hashCode <> " ? " <> importPath)
    assertNormalizedEqual
        "opportunistic fill should resolve the matching fallback"
        expr
        step2

    cachedAfterFill <- Directory.doesFileExist cacheFile
    assertBool "matching fallback should write the semantic cache" cachedAfterFill

    step3 <- Dhall.inputExpr ("missing " <> hashCode <> " ? 50")
    assertNormalizedEqual
        "after fill, resolution should come from the semantic cache"
        expr
        step3

    Directory.removeFile tempFile

-- | Opportunistic fill also works when the matching fallback is an env import.
envFillThenHitTest :: IO ()
envFillThenHitTest = withTempCache $ \cacheDir -> do
    originalVar <- Environment.lookupEnv "DHALL_TEST_VAR"

    let setVar = Environment.setEnv "DHALL_TEST_VAR" "42"

    let restoreVar =
            maybe
                (Environment.unsetEnv "DHALL_TEST_VAR")
                (Environment.setEnv "DHALL_TEST_VAR")
                originalVar

    bracket setVar (const restoreVar) $ \_ -> do
        expr <- Dhall.inputExpr "42"

        let hashCode = Import.hashExpressionToCode (Core.denote expr)
        let cacheFile = semanticCacheFile cacheDir expr

        step1 <- Dhall.inputExpr ("missing " <> hashCode <> " ? 10")
        assertNormalizedEqual
            "cache miss should use the literal fallback"
            (Core.NaturalLit 10)
            step1

        step2 <- Dhall.inputExpr ("missing " <> hashCode <> " ? env:DHALL_TEST_VAR")
        assertNormalizedEqual
            "env fallback should opportunistically fill the cache"
            expr
            step2

        cachedAfterFill <- Directory.doesFileExist cacheFile
        assertBool "env fallback should write the semantic cache" cachedAfterFill

        step3 <- Dhall.inputExpr ("missing " <> hashCode <> " ? 50")
        assertNormalizedEqual
            "after env fill, resolution should come from the semantic cache"
            expr
            step3

-- | A successful fallback whose hash does not match must not poison the cache.
hashMismatchDoesNotFillTest :: IO ()
hashMismatchDoesNotFillTest = withTempCache $ \cacheDir -> do
    expr <- Dhall.inputExpr "123"

    let wrongHashCode =
            "sha256:0000000000000000000000000000000000000000000000000000000011111111"

    tempFile <- writeTempImport "123"
    let importPath = relativeImport tempFile

    step1 <- Dhall.inputExpr ("missing " <> wrongHashCode <> " ? 10")
    assertNormalizedEqual
        "cache miss should use the literal fallback"
        (Core.NaturalLit 10)
        step1

    step2 <- inputExprNear tempFile ("missing " <> wrongHashCode <> " ? " <> importPath)
    assertNormalizedEqual
        "mismatched fallback should still resolve"
        expr
        step2

    let wrongCacheFile =
            cacheDir </> "dhall" </>
            "122000000000000000000000000000000000000000000000000000000011111111"

    cachedWrongHash <- Directory.doesFileExist wrongCacheFile
    assertBool "mismatched hash must not write a cache entry" (not cachedWrongHash)

    step3 <- Dhall.inputExpr ("missing " <> wrongHashCode <> " ? 50")
    assertNormalizedEqual
        "without a cache fill, the new fallback should be used"
        (Core.NaturalLit 50)
        step3

    Directory.removeFile tempFile

-- | A hash attached to the right of an already-successful alternative must not
-- be used for opportunistic caching.
hashOnRightIsIgnoredTest :: IO ()
hashOnRightIsIgnoredTest = withTempCache $ \cacheDir -> do
    expr <- Dhall.inputExpr "[ 123, 1 ]"

    let hashCode = Import.hashExpressionToCode (Core.denote expr)
    let cacheFile = semanticCacheFile cacheDir expr

    tempFile <- writeTempImport "[ 123, 1 ]"
    let importPath = relativeImport tempFile

    -- Left alternative succeeds, so the hash on the right is irrelevant.
    step2 <- inputExprNear tempFile (importPath <> " ? missing " <> hashCode)
    assertNormalizedEqual
        "successful left alternative should win"
        expr
        step2

    cached <- Directory.doesFileExist cacheFile
    assertBool
        "hash on the right of a successful alternative must not fill the cache"
        (not cached)

    expectedMiss <- Dhall.inputExpr "[ 50, 1 ]"
    step3 <- Dhall.inputExpr ("missing " <> hashCode <> " ? [ 50, 1 ]")
    assertNormalizedEqual
        "without a cache fill, the new fallback should be used"
        expectedMiss
        step3

    Directory.removeFile tempFile

-- | Unhashed Code fallbacks are no longer beta-normalized before inlining.
-- Opportunistic fill must still hash the normal form, or it would miss the
-- semantic cache (or write the wrong product).
fillFromUnnormalizedImportTest :: IO ()
fillFromUnnormalizedImportTest = withTempCache $ \cacheDir -> do
    expr <- Dhall.inputExpr "123"

    let hashCode = Import.hashExpressionToCode (Core.denote expr)
    let cacheFile = semanticCacheFile cacheDir expr

    tempFile <- writeTempImport "let x = 123 in x"
    let importPath = relativeImport tempFile

    step1 <- inputExprNear tempFile ("missing " <> hashCode <> " ? " <> importPath)
    assertNormalizedEqual
        "unnormalized fallback should still resolve to the semantic value"
        expr
        step1

    cachedAfterFill <- Directory.doesFileExist cacheFile
    assertBool
        "unnormalized matching fallback should write the semantic cache"
        cachedAfterFill

    step2 <- Dhall.inputExpr ("missing " <> hashCode <> " ? 50")
    assertNormalizedEqual
        "after fill, resolution should come from the semantic cache"
        expr
        step2

    Directory.removeFile tempFile
