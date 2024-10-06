{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

import Control.Monad.IO.Class     (liftIO)
import Data.Maybe                 (fromJust)
import Language.LSP.Test
import Language.LSP.Protocol.Types
    ( CompletionItem (..)
    , Diagnostic (..)
    , DiagnosticSeverity (..)
    , Hover (..)
    , MarkupContent (..)
    , Position (..)
    , Range (..)
    , toEither
    )
import Test.Tasty
import Test.Tasty.Hspec

#if MIN_VERSION_tasty_hspec(1,1,7)
import Test.Hspec
#endif

import qualified Data.Text as T
import qualified GHC.IO.Encoding
import qualified System.Environment as Environment

baseDir :: FilePath -> FilePath
baseDir d = "tests/fixtures/" <> d

hoveringSpec :: FilePath -> Spec
hoveringSpec dir =
  describe "Dhall.Hover"
    $ it "reports types on hover"
    $ runSession "dhall-lsp-server" fullCaps dir
    $ do
      docId <- openDoc "Types.dhall" "dhall"
      let typePos = Position 0 5
          functionPos = Position 2 7
          extractContents = toEither . _contents . fromJust
          getValue = T.unpack . _value
      typeHover <- getHover docId typePos
      funcHover <- getHover docId functionPos
      liftIO $ do
        case (extractContents typeHover, extractContents funcHover) of
          (Left typeContent, Left functionContent) -> do
            getValue typeContent `shouldBe` "Type"
            getValue functionContent `shouldBe` "\8704(_isAdmin : Bool) \8594 { home : Text, name : Text }"
          _ -> error "test failed"
        pure ()

lintingSpec :: FilePath -> Spec
lintingSpec fixtureDir =
  describe "Dhall.Lint" $ do
    it "reports unused bindings"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "UnusedBindings.dhall" "dhall"

        diags <- waitForDiagnosticsSource "Dhall.Lint"

        liftIO $ diags `shouldBe`
            [ Diagnostic
                { _range = Range
                    {_start = Position { _line = 2, _character = 10 }
                    , _end = Position { _line = 2, _character = 36 }
                    }
                , _severity = Just DiagnosticSeverity_Hint
                , _code = Nothing
                , _codeDescription = Nothing
                , _source = Just "Dhall.Lint"
                , _message = "Unused let binding 'bob'"
                , _tags = Nothing
                , _relatedInformation = Nothing
                , _data_ = Nothing
                }
            , Diagnostic
                { _range = Range
                    { _start = Position { _line = 4, _character = 11 }
                    , _end = Position { _line = 4, _character = 38 }
                    }
                , _severity = Just DiagnosticSeverity_Hint
                , _code = Nothing
                , _codeDescription = Nothing
                , _source = Just "Dhall.Lint"
                , _message = "Unused let binding 'carl'"
                , _tags = Nothing
                , _relatedInformation = Nothing
                , _data_ = Nothing
                }
            ]

        pure ()
    it "reports multiple hints"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "SuperfluousIn.dhall" "dhall"
        diags <- waitForDiagnosticsSource "Dhall.Lint"
        liftIO $ length diags `shouldBe` 2
        let diag1 = head diags
            diag2 = diags !! 1
        liftIO $ do
          _severity diag1 `shouldBe` Just DiagnosticSeverity_Hint
          T.unpack (_message diag1) `shouldContain` "Superfluous 'in'"
          _severity diag2 `shouldBe` Just DiagnosticSeverity_Hint
          T.unpack (_message diag2) `shouldContain` "Unused let binding"

codeCompletionSpec :: FilePath -> Spec
codeCompletionSpec fixtureDir =
  describe "Dhall.Completion" $ do
    it "suggests user defined types"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        docId <- openDoc "CustomTypes.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 2, _character = 35})
        liftIO $ do
          let firstItem = head cs
          _label firstItem `shouldBe` "Config"
          _detail firstItem `shouldBe` Just "Type"
    it "suggests user defined functions"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        docId <- openDoc "CustomFunctions.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 6, _character = 7})
        liftIO $ do
          let firstItem = head cs
          _label firstItem `shouldBe` "makeUser"
          _detail firstItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"
    it "suggests user defined bindings"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        docId <- openDoc "Bindings.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 59})
        liftIO $ do
          let firstItem = head cs
          _label firstItem `shouldBe` "bob"
          _detail firstItem `shouldBe` Just "Text"
    it "suggests functions from imports"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        docId <- openDoc "ImportedFunctions.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 33})
        liftIO $ do
          (firstItem, secondItem) <- case cs of
              [x, y] -> pure (x, y)
              _      -> fail "Unexpected number of items"
          _label firstItem `shouldBe` "`make user`"
          _label secondItem `shouldBe` "makeUser"
          _detail firstItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"
          _detail secondItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"
    it "suggests union alternatives"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        docId <- openDoc "Union.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 2, _character = 10})
        liftIO $ do
          (firstItem, secondItem) <- case cs of
              [x, y] -> pure (x, y)
              _      -> fail "Unexpected number of items"
          _label firstItem `shouldBe` "A"
          _label secondItem `shouldBe` "`B C`"
          _detail firstItem `shouldBe` Just "\8704(A : Text) \8594 < A : Text | `B C` >"
          _detail secondItem `shouldBe` Just "< A : Text | `B C` >"

diagnosticsSpec :: FilePath -> Spec
diagnosticsSpec fixtureDir = do
  describe "Dhall.TypeCheck" $ do
    it "reports unbound variables"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "UnboundVar.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Unbound variable"
    it "reports wrong type"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "WrongType.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Expression doesn't match annotation"
  describe "Dhall.Import" $ do
    it "reports invalid imports"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "InvalidImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Invalid input"
    it "reports missing imports"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "MissingImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Missing file"
  describe "Dhall.Parser"
    $ it "reports invalid syntax"
    $ runSession "dhall-lsp-server" fullCaps fixtureDir
    $ do
      _ <- openDoc "InvalidSyntax.dhall" "dhall"
      [diag] <- waitForDiagnosticsSource "Dhall.Parser"
      liftIO $ _severity diag `shouldBe` Just DiagnosticSeverity_Error

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  Environment.setEnv "TASTY_NUM_THREADS" "1"
  diagnostics <- testSpec "Diagnostics" (diagnosticsSpec (baseDir "diagnostics"))
  linting <- testSpec "Linting" (lintingSpec (baseDir "linting"))
  completion <- testSpec "Completion" (codeCompletionSpec (baseDir "completion"))
  hovering <- testSpec "Hovering" (hoveringSpec (baseDir "hovering"))
  defaultMain
    ( testGroup "Tests"
        [ diagnostics,
          linting,
          completion,
          hovering
        ]
    )
