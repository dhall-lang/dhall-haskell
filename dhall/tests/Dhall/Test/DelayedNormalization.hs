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
import qualified System.Timeout         as Timeout
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
        , testCase "List/length of unnormalized List/build iterate stays near-linear"
            listBuildIterateLengthIsNearLinearTest
        , testCase "List/length of user-lambda Church cons iterate stays near-linear"
            userLamChurchConsIterateLengthIsNearLinearTest
        , testCase "Church numeral plus-one evaluates without a thunk tower"
            churchNumeralPlusOneIsNearLinearTest
        , testCase "Natural/fold of (Natural → Natural) plus-one stays near-linear"
            funComposePlusOneIsNearLinearTest
        , testCase "Natural/fold shortcut on bounded types still fires"
            naturalFoldBoundedShortcutTest
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

-- | The evaluation Iterate benchmark imports Prelude @List/iterate@ without a
-- hash. After delayed Code normalization that import stays in the
-- @List/build@ / @cons (Natural/fold …)@ shape. Strict application of that
-- @cons@ forces every element while building the spine and turns
-- @List/length (iterate n …)@ into an O(n²) loop (OOM at n≈300000).
--
-- Builtin @VHLam@ application must keep list elements lazy so length only
-- pays for the spine — matching the β-normal @[f x] # xs@ encoding.
listBuildIterateLengthIsNearLinearTest :: IO ()
listBuildIterateLengthIsNearLinearTest = do
    -- Large enough that a quadratic implementation times out; small enough
    -- that the linear path finishes quickly under @tasty@.
    let n = 8000 :: Int
    let src =
            Text.pack $
                "List/length (List Natural)\n\
                \  ( List/build\n\
                \      (List Natural)\n\
                \      ( λ(list : Type) →\n\
                \        λ(cons : List Natural → list → list) →\n\
                \          List/fold\n\
                \            { index : Natural, value : {} }\n\
                \            ( List/indexed\n\
                \                {}\n\
                \                ( List/build\n\
                \                    {}\n\
                \                    ( λ(list : Type) →\n\
                \                      λ(cons : {} → list → list) →\n\
                \                        Natural/fold "
                    <> show n
                    <> " list (cons {=})\n\
                \                    )\n\
                \                )\n\
                \            )\n\
                \            list\n\
                \            ( λ(y : { index : Natural, value : {} }) →\n\
                \                cons\n\
                \                  ( Natural/fold\n\
                \                      y.index\n\
                \                      (List Natural)\n\
                \                      (λ(x : List Natural) → x # [ 1 ])\n\
                \                      [ 1 ]\n\
                \                  )\n\
                \            )\n\
                \      )\n\
                \  )\n"

    parsed <- Core.throws (Parser.exprFromText "list-build-iterate" src)

    mResult <- Timeout.timeout 2000000 $ do
        let resolved = fmap (\_ -> error "unexpected import") parsed
        let nf = Core.normalize (Core.denote resolved) :: Core.Expr Void Void
        assertEqual
            "List/length of the List/build iterate spine"
            (Core.NaturalLit (fromIntegral n))
            nf

    case mResult of
        Nothing ->
            assertFailure
                "timed out normalizing List/length of List/build iterate; \
                \strict List/build cons is likely forcing every Natural/fold"
        Just () ->
            return ()

-- | Same cost model as 'listBuildIterateLengthIsNearLinearTest', but the
-- outer cons is a *user* @VLam@ (@λ(x) → λ(acc) → [x] # acc@) driven by
-- 'Natural/fold' at an unbounded list type. The binder is @List _@, so
-- 'whnfCheapType' is false and 'vApp' does not force the element; a blanket
-- strict 'instantiate' would force each element's @Natural/fold@.
userLamChurchConsIterateLengthIsNearLinearTest :: IO ()
userLamChurchConsIterateLengthIsNearLinearTest = do
    let n = 8000 :: Int
    let src = Text.unlines
            [ "let n = " <> Text.pack (show n)
            , "let t = List (List Natural)"
            , "let succ ="
            , "      λ(acc : t) →"
            , "        [ Natural/fold (List/length (List Natural) acc) (List Natural) (λ(x : List Natural) → x # [ 1 ]) [ 1 ] ] # acc"
            , "let xs = Natural/fold n t succ ([] : t)"
            , "in  List/length (List Natural) xs"
            ]

    parsed <- Core.throws (Parser.exprFromText "user-lam-church-cons" src)

    mResult <- Timeout.timeout 2000000 $ do
        let resolved = fmap (\_ -> error "unexpected import") parsed
        let nf = Core.normalize (Core.denote resolved) :: Core.Expr Void Void
        assertEqual
            "List/length of user-lambda Church cons iterate"
            (Core.NaturalLit (fromIntegral n))
            nf

    case mResult of
        Nothing ->
            assertFailure
                "timed out; user-lambda Church cons is likely forcing each \
                \element Natural/fold (strict instantiate)"
        Just () ->
            return ()

-- | Scaled-down ChurchEval: nested Church multiplication applied to
-- @λ(x : Natural) → x + 1@. If 'vApp' does not bang a 'Natural' argument
-- *at the call site*, this builds a thunk tower and times out.
churchNumeralPlusOneIsNearLinearTest :: IO ()
churchNumeralPlusOneIsNearLinearTest = do
    let src = Text.unlines
            [ "let Nat = ∀(N : Type) → (N → N) → N → N"
            , "let n2 = λ(N : Type) → λ(s : N → N) → λ(z : N) → s (s z)"
            , "let n5 = λ(N : Type) → λ(s : N → N) → λ(z : N) → s (s (s (s (s z))))"
            , "let mul ="
            , "      λ(a : Nat) → λ(b : Nat) → λ(N : Type) → λ(s : N → N) → λ(z : N) →"
            , "        a N (b N s) z"
            , "let n10 = mul n2 n5"
            , "let n100 = mul n10 n10"
            , "let n1k = mul n10 n100"
            , "in  n1k Natural (λ(x : Natural) → x + 1) 0"
            ]

    parsed <- Core.throws (Parser.exprFromText "church-plus-one" src)

    mResult <- Timeout.timeout 2000000 $ do
        let resolved = fmap (\_ -> error "unexpected import") parsed
        let nf = Core.normalize (Core.denote resolved) :: Core.Expr Void Void
        assertEqual
            "Church numeral 1000 plus-one"
            (Core.NaturalLit 1000)
            nf

    case mResult of
        Nothing ->
            assertFailure
                "timed out; VLam Natural application is not call-site strict \
                \(ChurchEval thunk tower)"
        Just () ->
            return ()

-- | Scaled-down FunCompose: @Natural/fold@ at type @Natural → Natural@.
-- Function types are not 'boundedType'; the fold must still bang each
-- closure WHNF or this allocates a million thunks.
funComposePlusOneIsNearLinearTest :: IO ()
funComposePlusOneIsNearLinearTest = do
    let n = 100000 :: Int
    let src = Text.unlines
            [ "let compose ="
            , "      λ(f : Natural → Natural) → λ(g : Natural → Natural) →"
            , "      λ(x : Natural) → f (g x)"
            , "let n = " <> Text.pack (show n)
            , "in  Natural/fold n (Natural → Natural)"
            , "      (compose (λ(x : Natural) → x + 1))"
            , "      (λ(x : Natural) → x)"
            , "      0"
            ]

    parsed <- Core.throws (Parser.exprFromText "fun-compose-plus-one" src)

    mResult <- Timeout.timeout 2000000 $ do
        let resolved = fmap (\_ -> error "unexpected import") parsed
        let nf = Core.normalize (Core.denote resolved) :: Core.Expr Void Void
        assertEqual
            "Natural/fold compose plus-one"
            (Core.NaturalLit (fromIntegral n))
            nf

    case mResult of
        Nothing ->
            assertFailure
                "timed out; Natural/fold at a function type is not WHNF-strict \
                \(FunCompose thunk tower)"
        Just () ->
            return ()

-- | Identity on Natural is a fixed point, so a strict bounded fold must
-- return after one @succ@ rather than walking @n@ steps.
naturalFoldBoundedShortcutTest :: IO ()
naturalFoldBoundedShortcutTest = do
    let src = Text.unlines
            [ "Natural/fold 100000000 Natural (λ(x : Natural) → x) 7"
            ]

    parsed <- Core.throws (Parser.exprFromText "natural-fold-shortcut" src)

    mResult <- Timeout.timeout 2000000 $ do
        let resolved = fmap (\_ -> error "unexpected import") parsed
        let nf = Core.normalize (Core.denote resolved) :: Core.Expr Void Void
        assertEqual
            "bounded Natural/fold identity shortcuts"
            (Core.NaturalLit 7)
            nf

    case mResult of
        Nothing ->
            assertFailure
                "timed out; bounded Natural/fold shortcut is not firing"
        Just () ->
            return ()
