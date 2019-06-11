{-| This module contains everything related to how the LSP server handles
    diagnostic messages. -}
module Dhall.LSP.Handlers.Diagnostics
  ( compilerDiagnostics
  , publishDiagnostics
  , linterDiagnostics
  )
where

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Core     as LSP

import qualified Language.Haskell.LSP.Types    as J

import           Data.Text                      ( Text )

import           Dhall.LSP.Backend.Diagnostics
import           Dhall.LSP.Backend.Linting

diagnosisToLSP :: Diagnosis -> J.Diagnostic
diagnosisToLSP Diagnosis{..} = J.Diagnostic {..}
  where
    _range = case range of
      Just (Range (line1, col1) (line2, col2)) ->
        J.Range (J.Position line1 col1) (J.Position line2 col2)
      Nothing -> J.Range (J.Position 0 0) (J.Position 0 0)
    _severity = Just J.DsError
    _source = Just doctor
    _code = Nothing
    _message = diagnosis
    _relatedInformation = Nothing

compilerDiagnostics :: FilePath -> Text -> IO [J.Diagnostic]
compilerDiagnostics path txt = do
  errors <- runDhall path txt
  let diagnoses = concatMap (diagnose txt) errors
  return (map diagnosisToLSP diagnoses)

suggestionToDiagnostic :: Suggestion -> J.Diagnostic
suggestionToDiagnostic Suggestion {..} = J.Diagnostic {..}
  where
    _range = case range of
                Range (line1, col1) (line2, col2) ->
                  J.Range (J.Position line1 col1) (J.Position line2 col2)
    _severity = Just J.DsHint
    _source = Just "Dhall.Lint"
    _code = Nothing
    _message = suggestion
    _relatedInformation = Nothing

-- | Compute the list of possible improvements, as would be carried out by
--   @Dhall.Lint@.
linterDiagnostics :: Text -> [J.Diagnostic]
linterDiagnostics = map suggestionToDiagnostic . suggest

-- | Publish diagnostics for a given file. Overwrites any existing diagnostics
--   on the client side! In order to clear the diagnostics for a given file simply
--   pass the empty list [].
publishDiagnostics :: LSP.LspFuncs () -> J.Uri -> [J.Diagnostic] -> IO ()
publishDiagnostics lsp uri diags =
  LSP.sendFunc lsp $ LSP.NotPublishDiagnostics
    $ J.NotificationMessage "2.0" J.TextDocumentPublishDiagnostics
    $ J.PublishDiagnosticsParams uri (J.List diags)
