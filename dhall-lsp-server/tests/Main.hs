{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
  ( CompletionItem (..),
    Diagnostic (..),
    DiagnosticSeverity (..),
    Position (..),
  )
import Test.Tasty
import Test.Tasty.Hspec

baseDir :: FilePath -> FilePath
baseDir d = "tests/fixtures/" <> d

lintingSpec :: FilePath -> Spec
lintingSpec fixtureDir =
  describe "Dhall.Lint" $ do
    it "reports unused bindings"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "UnusedBindings.dhall" "dhall"
        diags <- waitForDiagnosticsSource "Dhall.Lint"
        _ <-
          liftIO $
            mapM
              ( \diag -> do
                  _severity diag `shouldBe` Just DsHint
                  T.unpack (_message diag) `shouldContain` "Unused let binding"
              )
              diags
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
          _severity diag1 `shouldBe` Just DsHint
          T.unpack (_message diag1) `shouldContain` "Superfluous 'in'"
          _severity diag2 `shouldBe` Just DsHint
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
          let firstItem = head cs
          _label firstItem `shouldBe` "makeUser"
          _detail firstItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"

diagnosticsSpec :: FilePath -> Spec
diagnosticsSpec fixtureDir = do
  describe "Dhall.TypeCheck" $ do
    it "reports unbound variables"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "UnboundVar.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ do
          _severity diag `shouldBe` Just DsError
          T.unpack (_message diag) `shouldContain` "Unbound variable"
    it "reports wrong type"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "WrongType.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ do
          _severity diag `shouldBe` Just DsError
          T.unpack (_message diag) `shouldContain` "Expression doesn't match annotation"
  describe "Dhall.Import" $ do
    it "reports invalid imports"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "InvalidImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ do
          _severity diag `shouldBe` Just DsError
          T.unpack (_message diag) `shouldContain` "Invalid input"
    it "reports missing imports"
      $ runSession "dhall-lsp-server" fullCaps fixtureDir
      $ do
        _ <- openDoc "MissingImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ do
          _severity diag `shouldBe` Just DsError
          T.unpack (_message diag) `shouldContain` "Missing file"
  describe "Dhall.Parser"
    $ it "reports invalid syntax"
    $ runSession "dhall-lsp-server" fullCaps fixtureDir
    $ do
      _ <- openDoc "InvalidSyntax.dhall" "dhall"
      [diag] <- waitForDiagnosticsSource "Dhall.Parser"
      liftIO $ _severity diag `shouldBe` Just DsError

main :: IO ()
main = do
  diagnostics <- testSpec "Diagnostics" (diagnosticsSpec (baseDir "diagnostics"))
  linting <- testSpec "Linting" (lintingSpec (baseDir "linting"))
  completion <- testSpec "Completion" (codeCompletionSpec (baseDir "completion"))
  defaultMain
    ( testGroup "Tests"
        [ diagnostics,
          linting,
          completion
        ]
    )
