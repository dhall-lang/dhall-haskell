
{-| This is the entry point for the LSP server. All calls are delegated to the haskell-lsp library
    which does the heavy lifting.
-}
module Dhall.LSP.Server(run) where

import           Control.Concurrent.STM.TVar
import           Data.Default
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core

import qualified Language.Haskell.LSP.Types            as J

import Data.Text (Text)
import qualified System.Log.Logger
import GHC.Conc (atomically)

import qualified Dhall.LSP.Handlers as Handlers
import qualified Dhall.LSP.Handlers.Command as Handlers
import qualified Dhall.LSP.Handlers.Hover as Handlers

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run mlog = do
  setupLogger mlog
  vlsp <- newTVarIO Nothing
  _    <- LSP.Control.run (makeConfig, initCallback vlsp) (lspHandlers vlsp)
                          lspOptions Nothing
  return ()
  where
    -- Callback that is called when the LSP server is started; makes the lsp
    -- state (LspFuncs) available to the message handlers through the vlsp TVar.
    initCallback
      :: TVar (Maybe (LSP.Core.LspFuncs ()))
      -> LSP.Core.LspFuncs ()
      -> IO (Maybe J.ResponseError)
    initCallback vlsp lsp = do
      atomically $ writeTVar vlsp (Just lsp)
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
                                "dhall.server.toJSON",
                                "dhall.server.annotateLet"]))
                 }

lspHandlers :: TVar (Maybe (LSP.Core.LspFuncs ())) -> LSP.Core.Handlers
lspHandlers lsp
  = def { LSP.Core.initializedHandler                       = Just $ wrapHandler lsp Handlers.nullHandler
        , LSP.Core.hoverHandler                             = Just $ wrapHandler lsp Handlers.hoverHandler
        , LSP.Core.didOpenTextDocumentNotificationHandler   = Just $ wrapHandler lsp Handlers.didOpenTextDocumentNotificationHandler
        , LSP.Core.didChangeTextDocumentNotificationHandler = Just $ wrapHandler lsp Handlers.nullHandler
        , LSP.Core.didSaveTextDocumentNotificationHandler   = Just $ wrapHandler lsp Handlers.didSaveTextDocumentNotificationHandler
        , LSP.Core.didCloseTextDocumentNotificationHandler  = Just $ wrapHandler lsp Handlers.nullHandler
        , LSP.Core.cancelNotificationHandler                = Just $ wrapHandler lsp Handlers.nullHandler
        , LSP.Core.responseHandler                          = Just $ wrapHandler lsp Handlers.nullHandler
        , LSP.Core.executeCommandHandler                    = Just $ wrapHandler lsp Handlers.executeCommandHandler
        , LSP.Core.documentFormattingHandler                = Just $ wrapHandler lsp Handlers.documentFormattingHandler
        }

-- Workaround to make our single-threaded LSP fit dhall-lsp's API, which
-- expects a multi-threaded implementation.
wrapHandler
  :: TVar (Maybe (LSP.Core.LspFuncs ()))
  -> (LSP.Core.LspFuncs () -> a -> IO ())
  -> a
  -> IO ()
wrapHandler vlsp handle message = do
  mlsp <- readTVarIO vlsp
  case mlsp of
    Just lsp -> handle lsp message
    Nothing ->
      fail "A handler was called before the LSP was initialized properly.\
          \ This should never happen."
