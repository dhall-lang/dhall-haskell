{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the disk cache used by unhashed Code and @as Source@
-- imports (@$XDG_CACHE_HOME/dhall-haskell-v2/@).
--
-- Code imports still typecheck in memory. The on-disk entry is a one-byte
-- "already type-checked" marker. Older cache files may still contain a small
-- encoded normal form, which the loader still accepts.
--
-- Unhashed @as Source@ imports store a distinct tag-2 Source-product payload
-- under a domain-separated key, so Code and Source entries cannot collide.
module Dhall.Test.SemisemanticCache where

import Control.Exception            (SomeException, bracket, try)
import Control.Monad                (void)
import Data.Foldable                (traverse_)
import Data.Void                    (Void)
import Data.Word                    (Word8)
import Dhall.Src                    (Src)
import System.FilePath              (takeDirectory, takeFileName, (</>))
import Test.Tasty                   (TestTree)
import Test.Tasty.HUnit             (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.ByteString    as ByteString
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text.IO
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Core         as Core
import qualified Dhall.Import       as Import
import qualified Dhall.Map
import qualified Lens.Micro
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import qualified System.IO.Temp     as Temp
import qualified Test.Tasty         as Tasty

getTests :: IO TestTree
getTests = return
    (Tasty.testGroup "Semisemantic cache"
        [ testCase "Typechecked marker is cached and reused" typecheckedMarkerCachedTest
        , testCase "Large NF stores a well-typed marker only" largeNFMarkerTest
        , testCase "Early-abort size check agrees with full walk" earlyAbortAgreesWithFullWalkTest
        , testCase "Child change invalidates parent cache" childChangeInvalidatesParentTest
        , testCase "as Source still evaluates" asSourceStillWorksTest
        , testCase "Source product is cached and reused" sourceProductCachedTest
        , testCase "Source cache is distinct from Code cache" sourceCacheDistinctFromCodeTest
        , testCase "Child source change invalidates parent Source cache" childSourceChangeInvalidatesParentTest
        , testCase "Substitution fingerprint invalidates Source cache" substitutionFingerprintInvalidatesSourceTest
        , testCase "Starting context fingerprint invalidates Source cache" startingContextFingerprintInvalidatesSourceTest
        , testCase "Hash-protected Code child uses Source edge hash" hashedCodeChildUsesSourceEdgeHashTest
        , testCase "Corrupt Source payload is ignored" corruptSourcePayloadIgnoredTest
        , testCase "Deep Source graph cache hit is stable" deepSourceGraphCacheHitTest
        , testCase "Overlapping Source parents share children safely" overlappingSourceParentsTest
        , testCase "ImportAlt falls back to Phase 1" importAltFallsBackTest
        , testCase "Env import identity invalidates Source cache" envImportInvalidatesSourceCacheTest
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

-- | Code imports store a typed marker (tag byte 1); normalization is delayed
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
            ("Code imports should be stored as typed markers in " <> file)
            (ByteString.singleton 1)
            bytes

-- | Large Text payload: store only a well-typed marker (tag byte 1, tiny file).
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
                ("large NF should be stored as typed marker in " <> file)
                (ByteString.singleton 1)
                bytes
            size <- Directory.getFileSize file
            assertBool "marker file must stay tiny" (size < 32)
        )
        files

    result2 <- inputFile tempFile
    assertNormalizedEqual "marker hit should still evaluate" result1 result2

    Directory.removeFile tempFile

-- | Early abort uses the same metric as a full walk, including the O(1)
-- Text-length case that @largeNFMarkerTest@ covers. The deep shared tree is
-- the case the abort actually saves work on (inlined giant NFs).
earlyAbortAgreesWithFullWalkTest :: IO ()
earlyAbortAgreesWithFullWalkTest = do
    let check label expr = do
            let full = Import.estimateExprSize expr
            let abort = Import.exceedsNFSizeThreshold expr
            assertEqual
                (label <> ": exceeds iff size > threshold")
                (full > Import.semisemanticNFSizeThreshold)
                abort

    check "small natural" (Core.NaturalLit 0)

    let smallTree = sharedNaturalPlusTree 10
    check "shared tree below threshold" smallTree
    assertBool
        "depth 10 should be below the 64KiB threshold"
        (Import.estimateExprSize smallTree < Import.semisemanticNFSizeThreshold)

    let bigText =
            Core.TextLit (Core.Chunks [] (Text.replicate 70000 "a"))
    check "large text payload" bigText

    assertBool
        "deep shared tree exceeds threshold"
        (Import.exceedsNFSizeThreshold (sharedNaturalPlusTree 20))

sharedNaturalPlusTree :: Int -> Core.Expr Void Void
sharedNaturalPlusTree depth = go depth
  where
    go 0 = Core.NaturalLit 0
    go n = let t = go (n - 1) in Core.NaturalPlus t t

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

-- | as Source path must keep working (does not use the Code NF semisemantic cache).
asSourceStillWorksTest :: IO ()
asSourceStillWorksTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-as-source" $ \dir -> do
        let childPath = dir </> "child.dhall"
        let parentPath = dir </> "parent.dhall"

        Text.IO.writeFile childPath "{ x = 1, y = 2 }"
        Text.IO.writeFile parentPath "(./child.dhall as Source).x"

        result <- inputFile parentPath
        expected <- Dhall.inputExpr "1"
        assertNormalizedEqual "as Source field projection" expected result

sourceProductTag :: Word8
sourceProductTag = 2

typedMarkerTag :: Word8
typedMarkerTag = 1

payloadTags :: FilePath -> IO [Word8]
payloadTags cacheDir = do
    files <- listCacheFiles cacheDir
    traverse readPayloadTag files

readPayloadTag :: FilePath -> IO Word8
readPayloadTag path = do
    bytes <- ByteString.readFile path
    case ByteString.uncons bytes of
        Just (tag, _) -> return tag
        Nothing -> assertFailure ("empty cache file: " <> path)

inputSourceFile :: FilePath -> IO (Core.Expr Src Void)
inputSourceFile path = do
    let settings =
            Lens.Micro.set Dhall.sourceName path
                ( Lens.Micro.set Dhall.rootDirectory (takeDirectory path)
                    Dhall.defaultInputSettings
                )
        file = Text.pack (takeFileName path)
    Dhall.inputExprWithSettings settings ("./" <> file <> " as Source")

-- | Unhashed as Source writes a tag-2 product and reloads equal.
sourceProductCachedTest :: IO ()
sourceProductCachedTest = withTempCache $ \cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-product" $ \dir -> do
        let path = dir </> "value.dhall"
        Text.IO.writeFile path "{ x = 1, y = 2 }"

        result1 <- inputSourceFile path
        tags1 <- payloadTags cacheDir
        assertBool "Source product tag-2 entry exists after first load"
            (sourceProductTag `elem` tags1)

        result2 <- inputSourceFile path
        expected <- Dhall.inputExpr "{ x = 1, y = 2 }"
        assertNormalizedEqual "first Source load" expected result1
        assertNormalizedEqual "cached Source reload" expected result2

-- | Warming Code cache then loading as Source must use a distinct tag/key.
sourceCacheDistinctFromCodeTest :: IO ()
sourceCacheDistinctFromCodeTest = withTempCache $ \cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-vs-code" $ \dir -> do
        let path = dir </> "value.dhall"
        Text.IO.writeFile path "{ x = 1, y = 2 }"

        _ <- inputFile path
        tagsAfterCode <- payloadTags cacheDir
        assertBool "Code writes a typed marker"
            (typedMarkerTag `elem` tagsAfterCode)
        assertBool "Code does not write a Source product"
            (sourceProductTag `notElem` tagsAfterCode)

        result <- inputSourceFile path
        tagsAfterSource <- payloadTags cacheDir
        assertBool "Source writes a distinct product tag"
            (sourceProductTag `elem` tagsAfterSource)
        assertBool "Source keeps the Code marker rather than replacing it"
            (typedMarkerTag `elem` tagsAfterSource)

        expected <- Dhall.inputExpr "{ x = 1, y = 2 }"
        assertNormalizedEqual "Source after Code warm" expected result

-- | Parent imports child as Source; changing child source must not reuse parent.
childSourceChangeInvalidatesParentTest :: IO ()
childSourceChangeInvalidatesParentTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-child" $ \dir -> do
        let childPath = dir </> "child.dhall"
        let parentPath = dir </> "parent.dhall"

        Text.IO.writeFile childPath "{ x = 1, y = 2 }"
        Text.IO.writeFile parentPath "(./child.dhall as Source).x"

        result1 <- inputFile parentPath
        expected1 <- Dhall.inputExpr "1"
        assertNormalizedEqual "initial Source child" expected1 result1

        Text.IO.writeFile childPath "{ x = 3, y = 2 }"

        result2 <- inputFile parentPath
        expected2 <- Dhall.inputExpr "3"
        assertNormalizedEqual "parent Source observes child change" expected2 result2

substitutionFingerprintInvalidatesSourceTest :: IO ()
substitutionFingerprintInvalidatesSourceTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-subst" $ \dir -> do
        let path = dir </> "value.dhall"
        Text.IO.writeFile path "N"

        let load n = do
                let settings =
                        Lens.Micro.set Dhall.substitutions
                            (Dhall.Map.singleton "N" (Core.NaturalLit n))
                            ( Lens.Micro.set Dhall.sourceName path
                                ( Lens.Micro.set Dhall.rootDirectory dir
                                    Dhall.defaultInputSettings
                                )
                            )
                Dhall.inputExprWithSettings settings "./value.dhall as Source"

        result1 <- load 1
        result2 <- load 2
        expected1 <- Dhall.inputExpr "1"
        expected2 <- Dhall.inputExpr "2"
        assertNormalizedEqual "substitution 1" expected1 result1
        assertNormalizedEqual "substitution 2" expected2 result2

startingContextFingerprintInvalidatesSourceTest :: IO ()
startingContextFingerprintInvalidatesSourceTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-context" $ \dir -> do
        let path = dir </> "id.dhall"
        Text.IO.writeFile path "λ(x : T) → x"

        let settingsWithT =
                Lens.Micro.set Dhall.startingContext
                    (Dhall.Context.insert "T" (Core.Const Core.Type) Dhall.Context.empty)
                    ( Lens.Micro.set Dhall.sourceName path
                        ( Lens.Micro.set Dhall.rootDirectory dir
                            Dhall.defaultInputSettings
                        )
                    )

        result <- Dhall.inputExprWithSettings settingsWithT "./id.dhall as Source"
        let expected =
                Core.Lam
                    mempty
                    (Core.makeFunctionBinding "x" (Core.Var "T"))
                    (Core.Var "x")
        assertNormalizedEqual "context-dependent Source load" expected result

        let settingsEmpty =
                Lens.Micro.set Dhall.sourceName path
                    ( Lens.Micro.set Dhall.rootDirectory dir
                        Dhall.defaultInputSettings
                    )
        failed <-
            try (Dhall.inputExprWithSettings settingsEmpty "./id.dhall as Source")
                :: IO (Either SomeException (Core.Expr Src Void))
        case failed of
            Left _ -> return ()
            Right _ ->
                assertFailure
                    "empty starting context must not reuse the T-context Source cache"

hashedCodeChildUsesSourceEdgeHashTest :: IO ()
hashedCodeChildUsesSourceEdgeHashTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-edge" $ \dir -> do
        let childPath = dir </> "child.dhall"
        let parentPath = dir </> "parent.dhall"
        let codeHash = Import.hashExpressionToCode (Core.NaturalLit 1)

        Text.IO.writeFile childPath "let x = 1 in x"
        Text.IO.writeFile parentPath ("./child.dhall " <> codeHash)

        let loadParentAsSource = do
                let settings =
                        Lens.Micro.set Dhall.sourceName parentPath
                            ( Lens.Micro.set Dhall.rootDirectory dir
                                Dhall.defaultInputSettings
                            )
                parsed <- Dhall.parseWithSettings settings "./parent.dhall as Source"
                Dhall.resolveWithSettings settings parsed

        result1 <- loadParentAsSource
        assertBool "first Source product is not the Code normal form"
            (Core.denote result1 /= (Core.NaturalLit 1 :: Core.Expr Void Void))

        Text.IO.writeFile childPath "1"

        result2 <- loadParentAsSource
        assertBool "same Code hash must not reuse a different Source product"
            (Core.denote result2 == (Core.NaturalLit 1 :: Core.Expr Void Void))

corruptSourcePayloadIgnoredTest :: IO ()
corruptSourcePayloadIgnoredTest = withTempCache $ \cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-corrupt" $ \dir -> do
        let path = dir </> "value.dhall"
        Text.IO.writeFile path "{ x = 1, y = 2 }"

        _ <- inputSourceFile path
        files <- listCacheFiles cacheDir
        traverse_
            (\file -> ByteString.writeFile file (ByteString.pack [sourceProductTag, 0xff, 0x00]))
            files

        result <- inputSourceFile path
        expected <- Dhall.inputExpr "{ x = 1, y = 2 }"
        assertNormalizedEqual "recompute after corrupt Source payload" expected result

deepSourceGraphCacheHitTest :: IO ()
deepSourceGraphCacheHitTest = withTempCache $ \cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-deep" $ \dir -> do
        Text.IO.writeFile (dir </> "leaf.dhall") "1"
        Text.IO.writeFile (dir </> "child.dhall") "./leaf.dhall"
        Text.IO.writeFile (dir </> "parent.dhall") "./child.dhall"

        let loadParent = do
                let settings =
                        Lens.Micro.set Dhall.sourceName (dir </> "parent.dhall")
                            ( Lens.Micro.set Dhall.rootDirectory dir
                                Dhall.defaultInputSettings
                            )
                Dhall.inputExprWithSettings settings "./parent.dhall as Source"

        result1 <- loadParent
        filesAfterFirst <- listCacheFiles cacheDir
        assertBool "deep Source graph writes cache entries"
            (not (null filesAfterFirst))

        result2 <- loadParent
        filesAfterSecond <- listCacheFiles cacheDir
        expected <- Dhall.inputExpr "1"
        assertNormalizedEqual "first deep Source load" expected result1
        assertNormalizedEqual "cached deep Source reload" expected result2
        assertEqual
            "reload should not write additional cache files"
            (length filesAfterFirst)
            (length filesAfterSecond)

overlappingSourceParentsTest :: IO ()
overlappingSourceParentsTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-overlap" $ \dir -> do
        let codeHash = Import.hashExpressionToCode (Core.NaturalLit 1)
        Text.IO.writeFile (dir </> "leaf.dhall") "1"
        Text.IO.writeFile
            (dir </> "hashed-leaf.dhall")
            ("./leaf.dhall " <> codeHash)
        Text.IO.writeFile (dir </> "a.dhall") "./hashed-leaf.dhall"
        Text.IO.writeFile (dir </> "b.dhall") "./hashed-leaf.dhall"
        Text.IO.writeFile (dir </> "parent.dhall") "[ ./a.dhall, ./b.dhall ]"

        let loadParent = do
                let settings =
                        Lens.Micro.set Dhall.sourceName (dir </> "parent.dhall")
                            ( Lens.Micro.set Dhall.rootDirectory dir
                                Dhall.defaultInputSettings
                            )
                Dhall.inputExprWithSettings settings "./parent.dhall as Source"

        result1 <- loadParent
        result2 <- loadParent
        expected <- Dhall.inputExpr "[ 1, 1 ]"
        assertNormalizedEqual "overlapping Source parents" expected result1
        assertNormalizedEqual "overlapping Source parents reload" expected result2

importAltFallsBackTest :: IO ()
importAltFallsBackTest = withTempCache $ \_cacheDir ->
    Temp.withSystemTempDirectory "dhall-source-alt" $ \dir -> do
        Text.IO.writeFile (dir </> "left-miss.dhall") "./missing.dhall ? 1"
        Text.IO.writeFile (dir </> "left-win.dhall") "1 ? ./missing.dhall"

        let load name = do
                let settings =
                        Lens.Micro.set Dhall.sourceName (dir </> name)
                            ( Lens.Micro.set Dhall.rootDirectory dir
                                Dhall.defaultInputSettings
                            )
                Dhall.inputExprWithSettings settings
                    ("./" <> Text.pack name <> " as Source")

        leftFail <- load "left-miss.dhall"
        leftWin <- load "left-win.dhall"
        expected <- Dhall.inputExpr "1"
        assertNormalizedEqual "ImportAlt missing left" expected leftFail
        assertNormalizedEqual "ImportAlt successful left" expected leftWin

envImportInvalidatesSourceCacheTest :: IO ()
envImportInvalidatesSourceCacheTest = withTempCache $ \_cacheDir -> do
    let var = "DHALL_SOURCE_MERKLE_ENV_TEST"
    original <- Environment.lookupEnv var
    let restore =
            maybe (Environment.unsetEnv var) (Environment.setEnv var) original

    bracket (Environment.setEnv var "1") (\_ -> restore) $ \_ -> do
        result1 <- Dhall.inputExpr "env:DHALL_SOURCE_MERKLE_ENV_TEST as Source"
        expected1 <- Dhall.inputExpr "1"
        assertNormalizedEqual "env Source first value" expected1 result1

        Environment.setEnv var "2"
        result2 <- Dhall.inputExpr "env:DHALL_SOURCE_MERKLE_ENV_TEST as Source"
        expected2 <- Dhall.inputExpr "2"
        assertNormalizedEqual "env Source observes change" expected2 result2


