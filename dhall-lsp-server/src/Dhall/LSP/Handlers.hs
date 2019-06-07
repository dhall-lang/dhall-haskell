module Dhall.LSP.Handlers where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Utility as LSP
import qualified Language.Haskell.LSP.VFS as LSP
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

import qualified Dhall.LSP.Handlers.Diagnostics as Diagnostics
import qualified Dhall.LSP.Handlers.DocumentFormatting as Handlers
import Dhall.LSP.Backend.Diagnostics

import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import qualified Network.URI.Encode as URI
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import qualified Yi.Rope as Rope

-- handler that doesn't do anything. Useful for example to make haskell-lsp shut
-- up about unhandled DidChangeTextDocument notifications (which are already
-- handled haskell-lsp itself).
nullHandler :: LSP.LspFuncs () -> a -> IO ()
nullHandler _ _ = return ()


{- Currently implemented by the dummy nullHandler:
initializedHandler :: LSP.LspFuncs () -> J.InitializedNotification -> IO ()

didChangeTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidChangeTextDocumentNotification -> IO ()

didCloseTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidCloseTextDocumentNotification -> IO ()

cancelNotificationHandler
  :: LSP.LspFuncs () -> J.CancelNotification -> IO ()

responseHandler :: LSP.LspFuncs () -> J.BareResponseMessage -> IO ()

executeCommandHandler :: LSP.LspFuncs () -> J.ExecuteCommandRequest -> IO ()
-}


-- This is a quick-and-dirty prototype implementation that will be completely
-- rewritten!
hoverHandler :: LSP.LspFuncs () -> J.HoverRequest -> IO ()
hoverHandler lsp request = do
  LSP.logs "LSP Handler: processing HoverRequest"
  let
    uri = request ^. J.params . J.textDocument . J.uri
    (J.Position line col) = request ^. (J.params . J.position)
    fileName = case J.uriToFilePath uri of
      Nothing -> fail "Failed to parse URI in ReqHover."
      Just path -> path
  txt <- readUri lsp uri
  errors <- runDhall fileName txt
  let
    explanations = mapMaybe (explain txt) errors
    isHovered :: Diagnosis -> Bool
    isHovered (Diagnosis _ Nothing _) = False
    isHovered (Diagnosis _ (Just (Range left right)) _) =
      left <= (line, col) && (line, col) <= right
    hover = case filter isHovered explanations of
      [] -> Nothing
      (diag : _) -> hoverFromDiagnosis diag
  LSP.sendFunc lsp $ LSP.RspHover $ LSP.makeResponseMessage request hover


hoverFromDiagnosis :: Diagnosis -> Maybe J.Hover
hoverFromDiagnosis (Diagnosis _ Nothing _) = Nothing
hoverFromDiagnosis (Diagnosis _ (Just (Range left right)) diagnosis) = Just
  J.Hover { .. }
  where
    _range =
      Just $ J.Range (uncurry J.Position left) (uncurry J.Position right)
    encodedDiag = URI.encode (Text.unpack diagnosis)
    command =
      "[Explain error](dhall-explain:?" <> Text.pack encodedDiag <> " )"
    _contents = J.List [J.PlainString command]


-- | Called by @didOpenTextDocumentNotificationHandler@ and
--   @didSaveTextDocumentNotificationHandler@.
diagnosticsHandler :: LSP.LspFuncs () -> J.Uri -> IO ()
diagnosticsHandler lsp uri = do
  LSP.logs $ "LSP Handler: processing diagnostics for " <> show uri
  let fileName = case J.uriToFilePath uri of
        Nothing -> fail "Failed to parse URI when computing diagnostics."
        Just path -> path
  txt <- readUri lsp uri
  diags <- Diagnostics.compilerDiagnostics fileName txt
  Diagnostics.publishDiagnostics lsp uri diags

didOpenTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidOpenTextDocumentNotification -> IO ()
didOpenTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidOpenTextDocumentNotification"
  let uri = notification ^. J.params . J.textDocument . J.uri
  diagnosticsHandler lsp uri


didSaveTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidSaveTextDocumentNotification -> IO ()
didSaveTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidSaveTextDocumentNotification"
  let uri = notification ^. J.params . J.textDocument . J.uri
  diagnosticsHandler lsp uri


documentFormattingHandler
  :: LSP.LspFuncs () -> J.DocumentFormattingRequest -> IO ()
documentFormattingHandler lsp request = do
  LSP.logs "LSP Handler: processing DocumentFormattingRequest"
  let uri = request ^. J.params . J.textDocument . J.uri
  formattedDocument <- flip runReaderT lsp
    $ Handlers.formatDocument uri undefined undefined
  LSP.sendFunc lsp $ LSP.RspDocumentFormatting $ LSP.makeResponseMessage
    request
    formattedDocument

-- helper function to query haskell-lsp's VFS
readUri :: LSP.LspFuncs () -> J.Uri -> IO Text
readUri lsp uri = do
  asd <- LSP.getVirtualFileFunc lsp uri
  case asd of
    Just (LSP.VirtualFile _ rope) -> return (Rope.toText rope)
    Nothing -> fail $ "Could not find " <> show uri <> " in VFS."
