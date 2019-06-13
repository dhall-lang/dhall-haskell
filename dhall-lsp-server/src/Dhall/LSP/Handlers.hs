module Dhall.LSP.Handlers where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Utility as LSP

import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

import qualified Dhall.LSP.Handlers.Diagnostics as Diagnostics
import qualified Dhall.LSP.Handlers.DocumentFormatting as Formatting

import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)

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
-}

didOpenTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidOpenTextDocumentNotification -> IO ()
didOpenTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidOpenTextDocumentNotification"
  let uri = notification ^. J.params . J.textDocument . J.uri
  Diagnostics.diagnosticsHandler lsp uri

didSaveTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidSaveTextDocumentNotification -> IO ()
didSaveTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidSaveTextDocumentNotification"
  let uri = notification ^. J.params . J.textDocument . J.uri
  Diagnostics.diagnosticsHandler lsp uri

documentFormattingHandler
  :: LSP.LspFuncs () -> J.DocumentFormattingRequest -> IO ()
documentFormattingHandler lsp request = do
  LSP.logs "LSP Handler: processing DocumentFormattingRequest"
  let uri = request ^. J.params . J.textDocument . J.uri
  formattedDocument <- flip runReaderT lsp
    $ Formatting.formatDocument uri undefined undefined
  LSP.sendFunc lsp $ LSP.RspDocumentFormatting $ LSP.makeResponseMessage
    request
    formattedDocument
