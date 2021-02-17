{-# LANGUAGE RecordWildCards #-}

{-| This is the entry point for the LSP server. -}
module Dhall.LSP.Server(run) where

import Control.Concurrent.MVar
import Control.Lens            ((^.))
import Data.Aeson              (Result (Success), fromJSON)
import Data.Default
import Data.Text               (Text)
import Dhall.LSP.Handlers
    ( completionHandler
    , didOpenTextDocumentNotificationHandler
    , didSaveTextDocumentNotificationHandler
    , documentFormattingHandler
    , documentLinkHandler
    , executeCommandHandler
    , hoverHandler
    , nullHandler
    , wrapHandler
    )
import Dhall.LSP.State

import qualified Language.LSP.Control    as LSP.Control
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import qualified System.Log.Logger

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run mlog = do
  setupLogger mlog
  state <- newEmptyMVar

  let onInitialConfiguration :: J.InitializeRequest -> Either Text ServerConfig
      onInitialConfiguration req
        | Just initOpts <- req ^. J.params . J.initializationOptions
        , Success config <- fromJSON initOpts
        = Right config
      onInitialConfiguration _ = Right def

  let onConfigurationChange :: J.DidChangeConfigurationNotification -> Either Text ServerConfig
      onConfigurationChange notification
        | preConfig <- notification ^. J.params . J.settings
        , Success config <- fromJSON preConfig
        = Right config
      onConfigurationChange _ = Right def

  -- Callback that is called when the LSP server is started; makes the lsp
  -- state (LspFuncs) available to the message handlers through the `state` MVar.
  let onStartup :: LSP.LspFuncs ServerConfig -> IO (Maybe J.ResponseError)
      onStartup lsp = do
        putMVar state (initialState lsp)
        return Nothing

  _ <- LSP.Control.run (LSP.InitializeCallbacks {..})
                       (lspHandlers state)
                       lspOptions
                       Nothing
  return ()

-- | sets the output logger.
-- | if no filename is provided then logger is disabled, if input is string `[OUTPUT]` then log goes to stderr,
-- | which then redirects inside VSCode to the output pane of the plugin.
setupLogger :: Maybe FilePath -> IO () -- TODO: ADD verbosity
setupLogger Nothing          = pure ()
setupLogger (Just "[OUTPUT]") = LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLogger file              = LSP.setupLogger file [] System.Log.Logger.DEBUG


-- Tells the LSP client to notify us about file changes. Handled behind the
-- scenes by haskell-lsp (in Language.LSP.VFS); we don't handle the
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
lspOptions :: LSP.Options
lspOptions = def { LSP.textDocumentSync = Just syncOptions
                 , LSP.completionTriggerCharacters = Just [':', '.', '/']
                 -- Note that this registers the dhall.server.lint command
                 -- with VSCode, which means that our plugin can't expose a
                 -- command of the same name. In the case of dhall.lint we
                 -- name the server-side command dhall.server.lint to work
                 -- around this peculiarity.
                 , LSP.executeCommandCommands =
                     Just
                       [ "dhall.server.lint",
                         "dhall.server.annotateLet",
                         "dhall.server.freezeImport",
                         "dhall.server.freezeAllImports"
                       ]
                 }

lspHandlers :: MVar ServerState -> LSP.Handlers
lspHandlers state
  = def { LSP.initializedHandler                       = Just $ wrapHandler state nullHandler
        , LSP.hoverHandler                             = Just $ wrapHandler state hoverHandler
        , LSP.didOpenTextDocumentNotificationHandler   = Just $ wrapHandler state didOpenTextDocumentNotificationHandler
        , LSP.didChangeTextDocumentNotificationHandler = Just $ wrapHandler state nullHandler
        , LSP.didSaveTextDocumentNotificationHandler   = Just $ wrapHandler state didSaveTextDocumentNotificationHandler
        , LSP.didCloseTextDocumentNotificationHandler  = Just $ wrapHandler state nullHandler
        , LSP.cancelNotificationHandler                = Just $ wrapHandler state nullHandler
        , LSP.responseHandler                          = Just $ wrapHandler state nullHandler
        , LSP.executeCommandHandler                    = Just $ wrapHandler state executeCommandHandler
        , LSP.documentFormattingHandler                = Just $ wrapHandler state documentFormattingHandler
        , LSP.documentLinkHandler                      = Just $ wrapHandler state documentLinkHandler
        , LSP.completionHandler                        = Just $ wrapHandler state completionHandler
        }
