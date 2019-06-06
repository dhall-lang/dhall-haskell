module LSP.Handlers where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Utility as LSP
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

import qualified LSP.Handlers.Diagnostics as Handlers
import qualified LSP.Handlers.DocumentFormatting as Handlers
import Backend.Dhall.Diagnostics

import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import qualified Data.Text.IO
import qualified Network.URI.Encode as URI
import qualified Data.Text as Text
import Data.Maybe (mapMaybe)

-- handler that doesn't do anything. Useful for example to make haskell-lsp shut
-- up about unhandled DidChangeTextDocument notifications (which are already
-- handled haskell-lsp itself).
nullHandler :: LSP.LspFuncs () -> a -> IO ()
nullHandler _ _ = return ()

initializedHandler :: LSP.LspFuncs () -> J.InitializedNotification -> IO ()
initializedHandler _lsp _notification = do
  LSP.logs "LSP Handler: processing InitializedNotification"
  return ()

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
  txt <- Data.Text.IO.readFile fileName
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

didOpenTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidOpenTextDocumentNotification -> IO ()
didOpenTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidOpenTextDocumentNotification"
  let
    uri = notification ^. J.params . J.textDocument . J.uri
    version = notification ^. J.params . J.textDocument . J.version
  LSP.logs $ "\turi=" <> show uri <> " version: " <> show version
  flip runReaderT lsp $ Handlers.sendDiagnostics uri (Just version)


didSaveTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidSaveTextDocumentNotification -> IO ()
didSaveTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidSaveTextDocumentNotification"
  let
    uri = notification ^. J.params . J.textDocument . J.uri
  LSP.logs $ "\turi=" <> show uri
  flip runReaderT lsp $ Handlers.sendDiagnostics uri Nothing

{- didChangeTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidChangeTextDocumentNotification -> IO ()
-}

didCloseTextDocumentNotificationHandler
  :: LSP.LspFuncs () -> J.DidCloseTextDocumentNotification -> IO ()
didCloseTextDocumentNotificationHandler lsp notification = do
  LSP.logs "LSP Handler: processing DidCloseTextDocumentNotification"
  let
    uri = notification ^. J.params . J.textDocument . J.uri
  LSP.logs $ "\turi=" <> show uri
  flip runReaderT lsp $ Handlers.sendEmptyDiagnostics uri Nothing

{- cancelNotificationHandler
  :: LSP.LspFuncs () -> J.CancelNotification -> IO ()
-}

responseHandler :: LSP.LspFuncs () -> J.BareResponseMessage -> IO ()
responseHandler _lsp response =
  LSP.logs $ "LSP Handler: Ignoring ResponseMessage: " ++ show response

executeCommandHandler :: LSP.LspFuncs () -> J.ExecuteCommandRequest -> IO ()
executeCommandHandler _lsp request =
  LSP.logs $ "LSP Handler: Ignoring ExecuteCommandRequest: " ++ show request

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
