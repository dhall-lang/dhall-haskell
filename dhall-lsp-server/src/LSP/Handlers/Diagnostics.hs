{-| This module contains everything related on how LSP server handles diagnostic messages. -}
module LSP.Handlers.Diagnostics( sendEmptyDiagnostics
                               , sendDiagnostics
                               ) where

import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Diagnostics
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Language.Haskell.LSP.Utility  as LSP.Utility


import qualified Data.Aeson                            as J
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J

import qualified System.IO.Unsafe
import qualified Data.Text.IO
import qualified Data.SortedList
import qualified Data.Map.Strict as Map

import Backend.Dhall.Diagnostics



-- TODO: Make max number of errors parameter configurable (not rly relevant since we got 1, but still) 
-- ---------------------------------------------------------------------
-- Y no method to flush particular source errors?
sendEmptyDiagnostics ::  J.Uri -> Maybe Int -> ReaderT (LSP.Core.LspFuncs ()) IO ()
sendEmptyDiagnostics fileUri version =
  publishDiagnostics 10 fileUri version defaultDiagnosticBySource

defaultDiagnosticBySource :: DiagnosticsBySource
defaultDiagnosticBySource = Map.singleton (Just defaultDiagnosticSource) (Data.SortedList.toSortedList [])

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification

sendDiagnostics :: J.Uri -> Maybe Int ->  ReaderT (LSP.Core.LspFuncs ()) IO ()
sendDiagnostics fileUri version = do
  let
   filePath = maybe (error "can't convert uri to file path") id $ J.uriToFilePath fileUri -- !FIXME: handle non-file uris
  txt <- lift $ Data.Text.IO.readFile filePath
  diags' <- lift $ compilerDiagnostics filePath txt
  lift $ LSP.Utility.logs $ "diagnostic: " <> show diags'
  publishDiagnostics 10 fileUri version (Map.union (partitionBySource diags') defaultDiagnosticBySource)


publishDiagnostics :: Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> ReaderT (LSP.Core.LspFuncs ()) IO ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  liftIO $ (LSP.Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

