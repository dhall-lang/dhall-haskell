
{-| This is the entry point for the LSP server. All calls are delegated to the haskell-lsp library
    which does the heavy lifting.
-}
module LSP.Server(run) where

import           Control.Concurrent(forkIO)
import           Control.Concurrent.STM.TChan
import qualified GHC.IO.Exception
import qualified Control.Exception
import           Data.Default
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import qualified Language.Haskell.LSP.Utility  as LSP.Utility

import qualified Data.Aeson                            as J
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J

import qualified System.Log.Logger

import LSP.Dispatcher(dispatcher) 

run :: Maybe String -> IO () -> IO Int
run maybeLog dispatcherProc = flip  Control.Exception.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan FromClientMessage)

  let
    dp lf = do
      _rpid  <- forkIO $ dispatcher lf rin
      dispatcherProc
      pure Nothing

  flip Control.Exception.finally finalProc $ do
    setupLogger maybeLog
    LSP.Control.run (pure (Right ()), dp) (lspHandlers rin) lspOptions Nothing
    -- TODO: CTRL.run takes logger as the last option, and should write LSP log into it
    -- TODO: 1. make upstream logger write in the format which lsp-inspector can read
    -- TODO: 2. it would be cool, if we start writing log on file creation, e.g.
    -- TODO:    e.g. "touch file /var/log/dhall-lsp-server/log-2018-03-12-12-45-34-fe5dk3.log to enable LSP logging"

  where
    handlers = [ Control.Exception.Handler ioExcept
               , Control.Exception.Handler someExcept
               ]
    finalProc = System.Log.Logger.removeAllHandlers
    ioExcept   (e :: Control.Exception.IOException)       = error $ show $ e -- print e >> pure 1
    someExcept (e :: Control.Exception.SomeException)     = error $ show $ e -- print e >> pure 1

-- ---------------------------------------------------------------------


-- | sets the output logger.
-- | if no filename is provided then logger is disabled, if input is string `[OUTPUT]` then log goes to stderr,
-- | which then redirects inside VSCode to the output pane of the plugin.
setupLogger :: Maybe FilePath -> IO () -- TODO: ADD verbosity
setupLogger Nothing          = pure ()
setupLogger (Just "[OUTPUT]") = LSP.Core.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLogger file              = LSP.Core.setupLogger file [] System.Log.Logger.DEBUG



syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncNone--J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

-- Capabilities entry point
lspOptions :: LSP.Core.Options
lspOptions = def { LSP.Core.textDocumentSync = Just syncOptions
                 -- , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-hello-command"]))
                 -- , Core.codeLensProvider = Just (J.CodeLensOptions (Just False))
                 }

lspHandlers :: TChan FromClientMessage -> LSP.Core.Handlers
lspHandlers rin
  = def { LSP.Core.initializedHandler                       = Just $ passHandler rin NotInitialized
        -- , Core.renameHandler                            = Just $ passHandler rin ReqRename
        -- , Core.hoverHandler                             = Just $ passHandler rin ReqHover
        , LSP.Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
        , LSP.Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
        , LSP.Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
        , LSP.Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
        , LSP.Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
        , LSP.Core.responseHandler                          = Just $ responseHandlerCb rin
        -- , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
        , LSP.Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
        , LSP.Core.documentFormattingHandler                = Just $ passHandler rin ReqDocumentFormatting
        }

-- ---------------------------------------------------------------------

passHandler :: TChan FromClientMessage -> (a -> FromClientMessage) -> LSP.Core.Handler a
passHandler rin convert notification =
  atomically $ writeTChan rin  (convert notification)

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan FromClientMessage -> LSP.Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp =
  LSP.Utility.logs $ ">>> got ignoring ResponseMessage:" ++ show resp

-- ---------------------------------------------------------------------


