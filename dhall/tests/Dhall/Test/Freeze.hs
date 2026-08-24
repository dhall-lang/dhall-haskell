{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Freeze where

import Data.Text    (Text)
import Data.Void    (Void)
import Dhall.Freeze (Intent (..), Scope (..))
import Test.Tasty   (TestTree)

import qualified Control.Exception as Exception
import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text.IO
import qualified Dhall.Core       as Core
import qualified Dhall.Freeze     as Freeze
import qualified Dhall.Import     as Import
import qualified Dhall.Parser     as Parser
import qualified Dhall.Test.Util  as Test.Util
import qualified Dhall.TypeCheck  as TypeCheck
import qualified Control.Monad.Trans.State.Strict as State
import qualified Lens.Micro       as Lens
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath    as FilePath
import qualified System.IO.Temp     as Temp
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Turtle

freezeDirectoryCached :: FilePath
freezeDirectoryCached = "./tests/freeze/cached"

freezeDirectorySecure :: FilePath
freezeDirectorySecure = "./tests/freeze/secure"

getTests :: IO TestTree
getTests = do
    freezeCachedTests <- Test.Util.discover (Turtle.chars <* "A.dhall")
        (freezeTest freezeDirectoryCached Cache)
        (Turtle.lstree freezeDirectoryCached)
    freezeSecureTests <- Test.Util.discover (Turtle.chars <* "A.dhall")
        (freezeTest freezeDirectorySecure Secure)
        (Turtle.lstree freezeDirectorySecure)

    let testTree = Tasty.testGroup "freeze tests"
            [ Tasty.testGroup "cached" [freezeCachedTests]
            , Tasty.testGroup "secure" [freezeSecureTests]
            , Tasty.HUnit.testCase
                "frozen as Source hash survives transitive freezing"
                sourceHashSurvivesTransitiveFreezing
            , Tasty.HUnit.testCase
                "nested frozen child survives as Source"
                nestedFrozenChildSurvivesAsSource
            , Tasty.HUnit.testCase
                "nested frozen child with relative imports survives as Source"
                nestedFrozenChildWithRelativeImportsSurvivesAsSource
            , Tasty.HUnit.testCase
                "nested frozen child survives as Source hash command"
                nestedFrozenChildSurvivesAsSourceHash
            ]

    return testTree

freezeTest :: FilePath -> Intent -> Text -> TestTree
freezeTest dir intent prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> "A.dhall")
        let outputFile = Text.unpack (prefix <> "B.dhall")

        inputText <- Text.IO.readFile inputFile

        parsedInput <- Core.throws (Parser.exprFromText mempty inputText)

        actualExpression <- Freeze.freezeExpression dir AllImports intent parsedInput

        let actualText = Core.pretty actualExpression <> "\n"

        expectedText <- Text.IO.readFile outputFile

        let message = "The linted expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedText actualText

-- | Prevent regression with @as Source@ when a transitive dependency is frozen *after*
-- the parent was already frozen and saved.
--
-- The test creates a temporary directory with the following files:
--
-- @
-- directory/
--   library.dhall   has contents:  ./function.dhall
--   function.dhall  has contents:  let x = 1 in x
-- @
--
-- The import expression is @./library.dhall as Source@.
-- 
-- Initially the imports are not frozen. The test goes through the following steps:
--
-- Step 1 — freeze and save the root expression.
--
-- Run @dhall freeze@ on the expression @./library.dhall as Source@. The result is a Dhall
-- expression @frozenMainText@. It has the form @./library.dhall sha256:... as Source@.
--
-- Step 2 — freeze a transitive file import on disk.
--
-- Run @dhall freeze@ on @library.dhall@ and overwrite the file on
-- disk. It will then contain @./function.dhall sha256:...@ instead of bare
-- @./function.dhall@.
--
-- Step 3 — resolve the expression @frozenMainText@ now.
--
-- Run import resolution on @frozenMainText@, which is the frozen expression from step 1
-- with sha256:... that was computed back then.
--
-- Step 4 — assert that the import-free result is @let x = 1 in x@. Freezing
-- @function.dhall@ on disk after the parent snapshot must not change the
-- semantics (or break resolution) of the earlier frozen @as Source@ import.
--
-- Fresh temporary cache directories are used for the freeze phase and the load
-- phase so the test does not rely on pre-existing semantic-cache entries.
sourceHashSurvivesTransitiveFreezing :: IO ()
sourceHashSurvivesTransitiveFreezing =
    Temp.withSystemTempDirectory "dhall-source-freeze" $ \directory ->
    Temp.withSystemTempDirectory "dhall-freeze-cache" $ \freezeCacheDir ->
    Temp.withSystemTempDirectory "dhall-load-cache" $ \loadCacheDir -> do
        let libraryText = "./function.dhall\n"
        let functionText = "let x = 1 in x\n"
        let mainText = "./library.dhall as Source"

        let libraryFile = directory FilePath.</> "library.dhall"
        let functionFile = directory FilePath.</> "function.dhall"

        Text.IO.writeFile libraryFile libraryText
        Text.IO.writeFile functionFile functionText

        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache cacheDir = Environment.setEnv "XDG_CACHE_HOME" cacheDir
            restoreCache = maybe
                (Environment.unsetEnv "XDG_CACHE_HOME")
                (Environment.setEnv "XDG_CACHE_HOME")
                originalCache

        frozenMainText <- Exception.bracket_ (setCache freezeCacheDir) restoreCache $ do
            parsedMain <- Core.throws (Parser.exprFromText "(input)" mainText)
            frozenMain <- Freeze.freezeExpression directory AllImports Secure parsedMain
            return (Core.pretty frozenMain <> "\n")

        Exception.bracket_ (setCache freezeCacheDir) restoreCache $ do
            parsedLibrary <- Core.throws (Parser.exprFromText libraryFile libraryText)
            frozenLibrary <- Freeze.freezeExpression directory AllImports Secure parsedLibrary
            Text.IO.writeFile libraryFile (Core.pretty frozenLibrary <> "\n")

        resolved <- Exception.bracket_ (setCache loadCacheDir) restoreCache $ do
            parsedFrozenMain <- Core.throws (Parser.exprFromText "(input)" frozenMainText)

            let status =
                    Lens.set Import.reportWarning (\_ -> return ())
                        (Import.emptyStatus directory)

            State.evalStateT (Test.Util.loadWith parsedFrozenMain) status

        expected <- Core.throws (Parser.exprFromText "(expected)" "let x = 1 in x")
        importFreeExpected <- Import.assertNoImports expected

        let actual = Core.normalize resolved :: Core.Expr Void Void
        let expectedNormalized = Core.normalize importFreeExpected :: Core.Expr Void Void

        let message = "Freezing a transitive child should not invalidate a parent frozen as Source import"

        Tasty.HUnit.assertEqual
            message
            expectedNormalized
            actual

-- | Prevent regression with @as Source@ when a nested frozen child is preserved
-- inside an @as Source@ root and resolved through the direct @loadWith@ path.
--
-- The test creates a temporary directory with the following files:
--
-- @
-- directory/
--   outer/package.dhall         has contents:  ./inner/package.dhall
--   outer/inner/package.dhall   has contents:  ./value.dhall sha256:...
--   outer/inner/value.dhall     has contents:  1
-- @
--
-- The import expression is @./outer/package.dhall as Source@.
--
-- The test goes through the following steps:
--
-- Step 1 — freeze the nested child.
--
-- Run @dhall freeze@ on @./value.dhall@ relative to @outer/inner/@. The result
-- is written to @outer/inner/package.dhall@, so that file contains the frozen
-- import @./value.dhall sha256:...@.
--
-- Step 2 — write the outer package and resolve the root.
--
-- Write @outer/package.dhall@ with the import @./inner/package.dhall@, then
-- resolve @./outer/package.dhall as Source@ with @loadWith@ from the top-level
-- temporary directory.
--
-- Step 3 — assert that the import-free result is @1@.
--
-- Prevents: @Missing file@ when a hashed import preserved inside a source
-- artifact loses the inner directory it was originally chained against and is
-- re-resolved from a parent directory that is too high in the tree.
--
-- Complements 'nestedFrozenChildSurvivesAsSourceHash', which exercises the same
-- nested bug through the full @dhall hash@ workflow.
--
-- A fresh temporary cache directory is used so the test starts without any
-- semantic-cache entries carried over from elsewhere.
nestedFrozenChildSurvivesAsSource :: IO ()
nestedFrozenChildSurvivesAsSource =
    Temp.withSystemTempDirectory "dhall-import-as-source-nested" $ \directory ->
    Temp.withSystemTempDirectory "dhall-import-as-source-cache" $ \cacheDir -> do
        let outerDirectory = directory FilePath.</> "outer"
        let innerDirectory = outerDirectory FilePath.</> "inner"

        Directory.createDirectoryIfMissing True innerDirectory

        let valueFile = innerDirectory FilePath.</> "value.dhall"
        let innerPackageFile = innerDirectory FilePath.</> "package.dhall"
        let outerPackageFile = outerDirectory FilePath.</> "package.dhall"
        Text.IO.writeFile valueFile "1\n"

        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache home = Environment.setEnv "XDG_CACHE_HOME" home
            restoreCache = maybe
                (Environment.unsetEnv "XDG_CACHE_HOME")
                (Environment.setEnv "XDG_CACHE_HOME")
                originalCache

        actualResolved <- Exception.bracket_ (setCache cacheDir) restoreCache $ do
            parsedFrozenChild <-
                Core.throws (Parser.exprFromText "(input)" "./value.dhall")

            frozenChild <-
                Freeze.freezeExpression innerDirectory AllImports Secure parsedFrozenChild

            Text.IO.writeFile innerPackageFile (Core.pretty frozenChild <> "\n")
            Text.IO.writeFile outerPackageFile "./inner/package.dhall\n"

            parsedMain <-
                Core.throws
                    (Parser.exprFromText "(input)" "./outer/package.dhall as Source")

            State.evalStateT
                (Test.Util.loadWith parsedMain)
                (Lens.set Import.reportWarning (\_ -> return ())
                    (Import.emptyStatus directory))

        expected <- Core.throws (Parser.exprFromText "(expected)" "1")
        expectedResolved <- Import.assertNoImports expected

        let message =
                "A frozen child preserved inside an as Source artifact should keep the base directory it was chained against"

        Tasty.HUnit.assertEqual
            message
            (Core.normalize expectedResolved :: Core.Expr Void Void)
            (Core.normalize actualResolved   :: Core.Expr Void Void)

-- | Like 'nestedFrozenChildSurvivesAsSource', but the hashed child itself
-- contains a relative import of a sibling file.
--
-- @
-- directory/
--   outer/package.dhall         has contents:  ./inner/package.dhall
--   outer/inner/package.dhall   has contents:  ./webhook.dhall sha256:...
--   outer/inner/webhook.dhall   has contents:  { clientConfig = ./config.dhall }
--   outer/inner/config.dhall    has contents:  { url = \"https://example.com\" }
-- @
--
-- Prevents: @Missing file@ when a later pass reloads the hashed child without
-- that file on the import stack, so @./config.dhall@ is chained against
-- @outer/package.dhall@ instead of @outer/inner/@. That is the
-- @large3.source@ failure with hashed @dhall-kubernetes@ type files such as
-- @MutatingWebhook.dhall@ importing @./WebhookClientConfig.dhall@.
nestedFrozenChildWithRelativeImportsSurvivesAsSource :: IO ()
nestedFrozenChildWithRelativeImportsSurvivesAsSource =
    Temp.withSystemTempDirectory "dhall-import-as-source-nested-rel" $ \directory ->
    Temp.withSystemTempDirectory "dhall-import-as-source-cache-rel" $ \cacheDir -> do
        let outerDirectory = directory FilePath.</> "outer"
        let innerDirectory = outerDirectory FilePath.</> "inner"

        Directory.createDirectoryIfMissing True innerDirectory

        let configFile = innerDirectory FilePath.</> "config.dhall"
        let webhookFile = innerDirectory FilePath.</> "webhook.dhall"
        let innerPackageFile = innerDirectory FilePath.</> "package.dhall"
        let outerPackageFile = outerDirectory FilePath.</> "package.dhall"

        Text.IO.writeFile configFile "{ url = \"https://example.com\" }\n"
        Text.IO.writeFile webhookFile "{ clientConfig = ./config.dhall }\n"

        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache home = Environment.setEnv "XDG_CACHE_HOME" home
            restoreCache = maybe
                (Environment.unsetEnv "XDG_CACHE_HOME")
                (Environment.setEnv "XDG_CACHE_HOME")
                originalCache

        actualResolved <- Exception.bracket_ (setCache cacheDir) restoreCache $ do
            parsedFrozenChild <-
                Core.throws (Parser.exprFromText "(input)" "./webhook.dhall")

            frozenChild <-
                Freeze.freezeExpression innerDirectory AllImports Secure parsedFrozenChild

            Text.IO.writeFile innerPackageFile (Core.pretty frozenChild <> "\n")
            Text.IO.writeFile outerPackageFile "./inner/package.dhall\n"

            parsedMain <-
                Core.throws
                    (Parser.exprFromText "(input)" "./outer/package.dhall as Source")

            State.evalStateT
                (Test.Util.loadWith parsedMain)
                (Lens.set Import.reportWarning (\_ -> return ())
                    (Import.emptyStatus directory))

        expected <- Core.throws
            (Parser.exprFromText "(expected)" "{ clientConfig = { url = \"https://example.com\" } }")
        expectedResolved <- Import.assertNoImports expected

        let message =
                "A hashed child with nested relative imports should resolve them against its own directory under as Source"

        Tasty.HUnit.assertEqual
            message
            (Core.normalize expectedResolved :: Core.Expr Void Void)
            (Core.normalize actualResolved   :: Core.Expr Void Void)

-- | Regression for the @dhall hash@ workflow with nested, already-frozen imports
-- inside an @as Source@ root.
--
-- The test creates a temporary directory with the following files:
--
-- @
-- directory/
--   pipeline.dhall              has contents:  ./outer/package.dhall as Source
--   outer/package.dhall         has contents:  ./inner/package.dhall
--   outer/inner/package.dhall   has contents:  ./value.dhall sha256:...
--   outer/inner/value.dhall     has contents:  1
-- @
--
-- The import expression is @./pipeline.dhall@.
--
-- The test goes through the following steps:
--
-- Step 1 — freeze the nested child.
--
-- Run @dhall freeze@ on @./value.dhall@ relative to @outer/inner/@ and write
-- the result to @outer/inner/package.dhall@, so that file contains the frozen
-- import @./value.dhall sha256:...@.
--
-- Step 2 — write the wrapper files.
--
-- Write @outer/package.dhall@ with @./inner/package.dhall@ and
-- @pipeline.dhall@ with @./outer/package.dhall as Source@.
--
-- Step 3 — run the @dhall hash@ workflow on @pipeline.dhall@.
--
-- Parse @pipeline.dhall@, resolve imports with @loadRelativeTo@, type-check,
-- normalize, and finally compute @hashExpressionToCode@.
--
-- Step 4 — assert that the resulting hash is the same as the hash of @1@.
--
-- Prevents: @Missing file@ (or wrong hash) when the preserve pass of @as Source@
-- embeds a hashed child using a relative path that was chained against an inner
-- directory but is later re-resolved from an outer file during finalize.
-- Without the fix, Dhall looked for @outer/value.dhall@ instead of
-- @outer/inner/value.dhall@ — the failure seen in @large3/pipeline.dhall@.
--
-- Complements 'sourceHashSurvivesTransitiveFreezing' (late freeze of a flat
-- child) and 'nestedFrozenChildSurvivesAsSource' (same nested bug via direct
-- @loadWith@).
--
-- A fresh temporary cache directory is used so the test starts without any
-- semantic-cache entries carried over from elsewhere.
nestedFrozenChildSurvivesAsSourceHash :: IO ()
nestedFrozenChildSurvivesAsSourceHash =
    Temp.withSystemTempDirectory "dhall-source-freeze-hash" $ \directory ->
    Temp.withSystemTempDirectory "dhall-freeze-hash-cache" $ \cacheDir -> do
        let outerDirectory = directory FilePath.</> "outer"
        let innerDirectory = outerDirectory FilePath.</> "inner"

        Directory.createDirectoryIfMissing True innerDirectory

        let valueFile = innerDirectory FilePath.</> "value.dhall"
        let innerPackageFile = innerDirectory FilePath.</> "package.dhall"
        let outerPackageFile = outerDirectory FilePath.</> "package.dhall"
        let pipelineFile = directory FilePath.</> "pipeline.dhall"

        Text.IO.writeFile valueFile "1\n"

        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache home = Environment.setEnv "XDG_CACHE_HOME" home
            restoreCache = maybe
                (Environment.unsetEnv "XDG_CACHE_HOME")
                (Environment.setEnv "XDG_CACHE_HOME")
                originalCache

        Exception.bracket_ (setCache cacheDir) restoreCache $ do
            parsedFrozenChild <-
                Core.throws (Parser.exprFromText "(input)" "./value.dhall")

            frozenChild <-
                Freeze.freezeExpression innerDirectory AllImports Secure parsedFrozenChild

            Text.IO.writeFile innerPackageFile (Core.pretty frozenChild <> "\n")
            Text.IO.writeFile outerPackageFile "./inner/package.dhall\n"
            Text.IO.writeFile pipelineFile "./outer/package.dhall as Source\n"

            pipelineText <- Text.IO.readFile pipelineFile

            parsedPipeline <-
                Core.throws (Parser.exprFromText pipelineFile pipelineText)

            resolved <-
                Import.loadRelativeTo directory Import.UseSemanticCache parsedPipeline

            _ <- Core.throws (TypeCheck.typeOf resolved)

            let normalized =
                    Core.alphaNormalize (Core.normalize resolved)

            let actualHash = Import.hashExpressionToCode normalized

            expected <- Core.throws (Parser.exprFromText "(expected)" "1")
            expectedResolved <- Import.assertNoImports expected

            let expectedHash =
                    Import.hashExpressionToCode
                        (Core.alphaNormalize (Core.normalize expectedResolved))

            let message =
                    "The dhall hash workflow should resolve a nested frozen child preserved inside an as Source import"

            Tasty.HUnit.assertEqual message expectedHash actualHash
