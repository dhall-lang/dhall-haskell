{-# LANGUAGE OverloadedStrings #-}

-- | Haskell-specific tests for delayed β-normalization of unhashed Code
-- imports. The language standard permits (but does not require) this
-- behavior; these cases lock in dhall-haskell's choice.
module Dhall.Test.DelayedNormalization where

import Control.Exception            (bracket)
import Data.Void                    (Void)
import Dhall.Src                    (Src)
import System.FilePath              (takeDirectory, takeFileName, (</>))
import Test.Tasty                   (TestTree)
import Test.Tasty.HUnit             (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import qualified Data.Text              as Text
import qualified Dhall.Binary           as Binary
import qualified Dhall.Core             as Core
import qualified Dhall.Freeze           as Freeze
import qualified Dhall.Import           as Import
import qualified Dhall.Parser           as Parser
import qualified System.Directory       as Directory
import qualified System.Environment     as Environment
import qualified System.IO.Temp         as Temp
import qualified Test.Tasty             as Tasty

getTests :: IO TestTree
getTests = return
    (Tasty.testGroup "Delayed normalization of unhashed imports"
        [ testCase "Unhashed Code is not β-normalized at resolve time"
            unhashedResolveIsNotNormalTest
        , testCase "Hashed Code is still inlined as β-normal form"
            hashedResolveIsNormalTest
        , testCase "freeze writes the αβ-normal form to the semantic cache"
            freezeWritesNormalizedCacheTest
        ])

withTempCache :: (FilePath -> IO a) -> IO a
withTempCache action =
    Temp.withSystemTempDirectory "dhall-delayed-nf" $ \cacheDir -> do
        originalCache <- Environment.lookupEnv "XDG_CACHE_HOME"

        let setCache = Environment.setEnv "XDG_CACHE_HOME" cacheDir

        let restoreCache =
                maybe
                    (Environment.unsetEnv "XDG_CACHE_HOME")
                    (Environment.setEnv "XDG_CACHE_HOME")
                    originalCache

        bracket setCache (const restoreCache) (\_ -> action cacheDir)

writeTempImport :: Text.Text -> IO FilePath
writeTempImport contents =
    Temp.writeTempFile "." "tmp.dhall" (Text.unpack contents)

relativeImport :: FilePath -> Text.Text
relativeImport path = "./" <> Text.pack (takeFileName path)

loadNear :: FilePath -> Text.Text -> IO (Core.Expr Src Void)
loadNear path exprText = do
    parsed <- Core.throws (Parser.exprFromText mempty exprText)
    Import.loadRelativeTo (takeDirectory path) Import.UseSemanticCache parsed

reducibleSource :: Text.Text
reducibleSource = "let x = 1 + 1 in x"

expectedNF :: Core.Expr Void Void
expectedNF = Core.NaturalLit 2

-- | Unhashed @Code@ inlines the typechecked tree, so @let x = 1 + 1 in x@
-- is still a @let@ after resolve. It is β-equivalent to @2@.
unhashedResolveIsNotNormalTest :: IO ()
unhashedResolveIsNotNormalTest = withTempCache $ \_cacheDir -> do
    tempFile <- writeTempImport reducibleSource
    loaded <- loadNear tempFile (relativeImport tempFile)

    let denoted = Core.denote loaded :: Core.Expr Void Void
    let nf = Core.normalize denoted :: Core.Expr Void Void

    assertBool
        "unhashed Code should still contain the let after resolve"
        (denoted /= nf)
    assertEqual
        "unhashed Code should be β-equivalent to 2"
        expectedNF
        nf

    Directory.removeFile tempFile

-- | The same file with a matching integrity check is reduced before inlining,
-- so resolve yields the β-normal form.
hashedResolveIsNormalTest :: IO ()
hashedResolveIsNormalTest = withTempCache $ \_cacheDir -> do
    tempFile <- writeTempImport reducibleSource

    let hashCode =
            Import.hashExpressionToCode (Core.alphaNormalize expectedNF)

    loaded <- loadNear tempFile (relativeImport tempFile <> " " <> hashCode)

    let denoted = Core.denote loaded :: Core.Expr Void Void

    assertEqual
        "hashed Code should be inlined as the β-normal form"
        expectedNF
        (Core.alphaNormalize denoted)

    Directory.removeFile tempFile

-- | @dhall freeze@ must store the αβ-normal form under the frozen hash, even
-- though @loadWith@ of the unprotected import is no longer β-normal.
freezeWritesNormalizedCacheTest :: IO ()
freezeWritesNormalizedCacheTest = withTempCache $ \cacheDir -> do
    tempFile <- writeTempImport reducibleSource

    let import_ =
            Core.Import
                { Core.importHashed =
                    Core.ImportHashed
                        { Core.hash = Nothing
                        , Core.importType =
                            Core.Local Core.Here Core.File
                                { Core.directory = Core.Directory []
                                , Core.file = Text.pack (takeFileName tempFile)
                                }
                        }
                , Core.importMode = Core.Code
                }

    frozen <- Freeze.freezeImport (takeDirectory tempFile) import_

    let expectedHash = Import.hashExpression (Core.alphaNormalize expectedNF)

    assertEqual
        "frozen hash should be the hash of the αβ-normal form"
        (Just expectedHash)
        (Core.hash (Core.importHashed frozen))

    let cacheFile =
            cacheDir </> "dhall" </> ("1220" <> show expectedHash)

    exists <- Directory.doesFileExist cacheFile
    assertBool
        "freeze should write a semantic-cache file"
        exists

    bytes <- ByteString.readFile cacheFile
    decoded <- case Binary.decodeExpression (ByteString.Lazy.fromStrict bytes) of
        Left err ->
            assertFailure ("failed to decode semantic-cache product: " <> show err)
        Right expression ->
            return (expression :: Core.Expr Void Void)

    assertEqual
        "semantic-cache product should be the αβ-normal form, not the let"
        expectedNF
        decoded

    Directory.removeFile tempFile
