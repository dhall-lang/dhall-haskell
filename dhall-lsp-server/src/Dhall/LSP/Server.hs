{-| This is the entry point for the LSP server. -}
module Dhall.LSP.Server(run) where

import           Control.Concurrent.MVar
import           Data.Default
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core

import qualified Language.Haskell.LSP.Types as J

import Data.Text (Text)
import qualified System.Log.Logger

import Dhall.LSP.State
import Dhall.LSP.Handlers (nullHandler, wrapHandler, hoverHandler,
  didOpenTextDocumentNotificationHandler, didSaveTextDocumentNotificationHandler,
  executeCommandHandler, documentFormattingHandler)

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run mlog = do
  setupLogger mlog
  state <- newEmptyMVar
  _    <- LSP.Control.run (makeConfig, initCallback state) (lspHandlers state)
                          lspOptions Nothing
  return ()
  where
    -- Callback that is called when the LSP server is started; makes the lsp
    -- state (LspFuncs) available to the message handlers through the vlsp MVar.
    initCallback
      :: MVar ServerState
      -> LSP.Core.LspFuncs ()
      -> IO (Maybe J.ResponseError)
    initCallback state lsp = do
      putMVar state (initialState lsp)
      return Nothing

    -- Interpret DidChangeConfigurationNotification; pointless at the moment
    -- since we don't use a configuration.
    makeConfig :: J.DidChangeConfigurationNotification -> Either Text ()
    makeConfig _ = Right ()

-- | sets the output logger.
-- | if no filename is provided then logger is disabled, if input is string `[OUTPUT]` then log goes to stderr,
-- | which then redirects inside VSCode to the output pane of the plugin.
setupLogger :: Maybe FilePath -> IO () -- TODO: ADD verbosity
setupLogger Nothing          = pure ()
setupLogger (Just "[OUTPUT]") = LSP.Core.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLogger file              = LSP.Core.setupLogger file [] System.Log.Logger.DEBUG


-- Tells the LSP client to notify us about file changes. Handled behind the
-- scenes by haskell-lsp (in Language.Haskell.LSP.VFS); we don't handle the
-- corresponding notifications ourselves.
syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

-- Server capabilities. Tells the LSP client that we can execute commands etc.
lspOptions :: LSP.Core.Options
lspOptions = def { LSP.Core.textDocumentSync = Just syncOptions
                 , LSP.Core.executeCommandProvider =
                     -- Note that this registers the dhall.server.lint command
                     -- with VSCode, which means that our plugin can't expose a
                     -- command of the same name. In the case of dhall.lint we
                     -- name the server-side command dhall.server.lint to work
                     -- around this peculiarity.
                     Just (J.ExecuteCommandOptions
                       (J.List ["dhall.server.lint",
                                "dhall.server.annotateLet"]))
                 }

lspHandlers :: MVar ServerState -> LSP.Core.Handlers
lspHandlers state
  = def { LSP.Core.initializedHandler                       = Just $ wrapHandler state nullHandler
        , LSP.Core.hoverHandler                             = Just $ wrapHandler state hoverHandler
        , LSP.Core.didOpenTextDocumentNotificationHandler   = Just $ wrapHandler state didOpenTextDocumentNotificationHandler
        , LSP.Core.didChangeTextDocumentNotificationHandler = Just $ wrapHandler state nullHandler
        , LSP.Core.didSaveTextDocumentNotificationHandler   = Just $ wrapHandler state didSaveTextDocumentNotificationHandler
        , LSP.Core.didCloseTextDocumentNotificationHandler  = Just $ wrapHandler state nullHandler
        , LSP.Core.cancelNotificationHandler                = Just $ wrapHandler state nullHandler
        , LSP.Core.responseHandler                          = Just $ wrapHandler state nullHandler
        , LSP.Core.executeCommandHandler                    = Just $ wrapHandler state executeCommandHandler
        , LSP.Core.documentFormattingHandler                = Just $ wrapHandler state documentFormattingHandler
        }
