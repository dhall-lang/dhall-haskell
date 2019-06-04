{-| This module contains everything related on how LSP server handles diagnostic messages. -}
module LSP.Handlers.Diagnostics( sendEmptyDiagnostics
                               , sendDiagnostics
                               ) where

-- import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Diagnostics
-- import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Language.Haskell.LSP.Utility  as LSP.Utility


import qualified Language.Haskell.LSP.Types            as LSP
-- import qualified Language.Haskell.LSP.Types.Lens       as LSP

-- import qualified System.IO.Unsafe
import qualified Data.Text.IO
import qualified Data.SortedList
import qualified Data.Map.Strict as Map
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (lift, liftIO)

import Data.Text (Text)

import Backend.Dhall.Diagnostics



-- TODO: Make max number of errors parameter configurable (not rly relevant since we got 1, but still) 
-- ---------------------------------------------------------------------
-- Y no method to flush particular source errors?
sendEmptyDiagnostics ::  LSP.Uri -> Maybe Int -> ReaderT (LSP.Core.LspFuncs ()) IO ()
sendEmptyDiagnostics fileUri version =
  publishDiagnostics 10 fileUri version defaultDiagnosticBySource

defaultDiagnosticBySource :: DiagnosticsBySource
defaultDiagnosticBySource = Map.singleton (Just "Dhall") (Data.SortedList.toSortedList [])

diagnosisToLSP :: Diagnosis -> LSP.Diagnostic
diagnosisToLSP Diagnosis{..} = LSP.Diagnostic {..}
  where
    _range = case range of
      Just (Range (line1, col1) (line2, col2)) ->
        LSP.Range (LSP.Position line1 col1) (LSP.Position line2 col2)
      Nothing -> LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)
    _severity = Just LSP.DsError
    -- TODO use @doctor@ instead. Seems incompatible with LSP.Core's API...
    _source = Just "Dhall"
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
sendDiagnostics :: LSP.Uri -> Maybe Int ->  ReaderT (LSP.Core.LspFuncs ()) IO ()
sendDiagnostics fileUri version = do
  let
   filePath = maybe (error "can't convert uri to file path") id $ LSP.uriToFilePath fileUri -- !FIXME: handle non-file uris
  txt <- lift $ Data.Text.IO.readFile filePath
  diags' <- lift $ compilerDiagnostics filePath txt
  lift $ LSP.Utility.logs $ "diagnostic: " <> show diags'
  publishDiagnostics 10 fileUri version (Map.union (partitionBySource diags') defaultDiagnosticBySource)


publishDiagnostics :: Int -> LSP.Uri -> LSP.TextDocumentVersion -> DiagnosticsBySource -> ReaderT (LSP.Core.LspFuncs ()) IO ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  liftIO $ (LSP.Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

