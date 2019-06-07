{-| This module contains everything related on how LSP server handles diagnostic messages. -}
module Dhall.LSP.Handlers.Diagnostics( compilerDiagnostics
                               , sendEmptyDiagnostics
                               , sendDiagnostics
                               ) where

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Language.Haskell.LSP.Utility  as LSP.Utility


import qualified Language.Haskell.LSP.Types            as LSP
-- import qualified Language.Haskell.LSP.Types.Lens       as LSP

-- import qualified System.IO.Unsafe
import qualified Data.Text.IO

import Data.Text (Text)

import Dhall.LSP.Backend.Diagnostics


sendEmptyDiagnostics :: LSP.LspFuncs () -> LSP.Uri -> IO ()
sendEmptyDiagnostics lsp fileUri =
  publishDiagnostics' lsp fileUri []

diagnosisToLSP :: Diagnosis -> LSP.Diagnostic
diagnosisToLSP Diagnosis{..} = LSP.Diagnostic {..}
  where
    _range = case range of
      Just (Range (line1, col1) (line2, col2)) ->
        LSP.Range (LSP.Position line1 col1) (LSP.Position line2 col2)
      Nothing -> LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)
    _severity = Just LSP.DsError
    _source = Just doctor
    _code = Nothing
    _message = diagnosis
    _relatedInformation = Nothing


compilerDiagnostics :: FilePath -> Text -> IO [LSP.Diagnostic]
compilerDiagnostics path txt = do
  errors <- runDhall path txt
  let diagnoses = concatMap (diagnose txt) errors
  return (map diagnosisToLSP diagnoses)
  
-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: LSP.LspFuncs () -> LSP.Uri -> IO ()
sendDiagnostics lsp fileUri = do
  let
   filePath = maybe (error "can't convert uri to file path") id $ LSP.uriToFilePath fileUri -- !FIXME: handle non-file uris
  txt <- Data.Text.IO.readFile filePath
  diags <- compilerDiagnostics filePath txt
  LSP.Utility.logs $ "diagnostic: " <> show diags
  publishDiagnostics' lsp fileUri diags


publishDiagnostics' :: LSP.LspFuncs () -> LSP.Uri -> [LSP.Diagnostic] -> IO ()
publishDiagnostics' lsp uri diags =
  LSP.sendFunc lsp $ LSP.NotPublishDiagnostics
    $ LSP.NotificationMessage "2.0" LSP.TextDocumentPublishDiagnostics
    $ LSP.PublishDiagnosticsParams uri (LSP.List diags)
