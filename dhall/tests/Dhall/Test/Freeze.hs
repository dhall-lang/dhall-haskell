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
import qualified Control.Monad.Trans.State.Strict as State
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
                    (Import.emptyStatus directory)
                        { Import._reportWarning = \_ -> return ()
                        }

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
