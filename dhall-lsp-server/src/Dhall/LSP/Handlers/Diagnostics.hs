{-| This module contains everything related to how the LSP server handles
    diagnostic messages. -}
module Dhall.LSP.Handlers.Diagnostics
  ( diagnosticsHandler, explainDiagnosis
  )
where

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Core     as LSP
import qualified Language.Haskell.LSP.Utility  as LSP
import qualified Language.Haskell.LSP.Types    as J

import           Data.Text                      ( Text )

import           Dhall.LSP.Backend.Diagnostics
import           Dhall.LSP.Backend.Linting
import           Dhall.LSP.Util (readUri)

import           Data.List                      ( find )
import           Data.Maybe                     ( mapMaybe )

-- | Called by @didOpenTextDocumentNotificationHandler@ and
--   @didSaveTextDocumentNotificationHandler@.
diagnosticsHandler :: LSP.LspFuncs () -> J.Uri -> IO ()
diagnosticsHandler lsp uri = do
  LSP.logs $ "LSP Handler: processing diagnostics for " <> show uri
  let fileName = case J.uriToFilePath uri of
        Nothing -> fail "Failed to parse URI when computing diagnostics."
        Just path -> path
  txt <- readUri lsp uri
  let lintDiags = linterDiagnostics txt
  compDiags <- compilerDiagnostics fileName txt
  publishDiagnostics lsp uri (compDiags ++ lintDiags)

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
  errors <- checkDhall path txt
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

explainDiagnosis :: FilePath -> Text -> Position -> IO (Maybe Diagnosis)
explainDiagnosis path txt pos = do
  errors <- checkDhall path txt
  let explanations = mapMaybe (explain txt) errors
  return $ find (isHovered pos) explanations

isHovered :: Position -> Diagnosis -> Bool
isHovered _ (Diagnosis _ Nothing _) = False
isHovered pos (Diagnosis _ (Just (Range left right)) _) =
  left <= pos && pos <= right

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
