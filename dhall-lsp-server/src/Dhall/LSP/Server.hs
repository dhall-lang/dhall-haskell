{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}

{-| This is the entry point for the LSP server. -}
module Dhall.LSP.Server(run) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (fromJSON)
import Data.Default
import Dhall.LSP.Handlers
    ( completionHandler
    , didOpenTextDocumentNotificationHandler
    , didSaveTextDocumentNotificationHandler
    , documentFormattingHandler
    , documentLinkHandler
    , executeCommandHandler
    , hoverHandler
    , initializedHandler
    , workspaceChangeConfigurationHandler
    , textDocumentChangeHandler
    , cancelationHandler
    )
import Dhall.LSP.State
import Language.LSP.Server (Options(..), ServerDefinition(..), type (<~>)(..))
import Language.LSP.Types
import System.Exit (ExitCode(..))

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified System.Exit as Exit
import qualified System.Log.Logger

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run mlog = do
  setupLogger mlog

  state <- MVar.newMVar initialState

  let defaultConfig = def

  let onConfigurationChange _oldConfig json =
        case fromJSON json of
            Aeson.Success config -> Right config
            Aeson.Error   string -> Left (Text.pack string)

  let doInitialize environment _request = do
          return (Right environment)

  let options = def
        { LSP.textDocumentSync = Just syncOptions

        , completionTriggerCharacters = Just [':', '.', '/']

        -- Note that this registers the dhall.server.lint command
        -- with VSCode, which means that our plugin can't expose a
        -- command of the same name. In the case of dhall.lint we
        -- name the server-side command dhall.server.lint to work
        -- around this peculiarity.
        , executeCommandCommands =
            Just
              [ "dhall.server.lint",
                "dhall.server.annotateLet",
                "dhall.server.freezeImport",
                "dhall.server.freezeAllImports"
              ]
        }

  let staticHandlers =
        mconcat
          [ hoverHandler
          , didOpenTextDocumentNotificationHandler
          , didSaveTextDocumentNotificationHandler
          , executeCommandHandler
          , documentFormattingHandler
          , documentLinkHandler
          , completionHandler
          , initializedHandler
          , workspaceChangeConfigurationHandler
          , textDocumentChangeHandler
          , cancelationHandler
          ]

  let interpretHandler environment = Iso{..}
        where
          forward :: HandlerM a -> IO a
          forward handler =
            MVar.modifyMVar state \oldState -> do
              LSP.runLspT environment do
                (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                result <- case e of
                  Left (Log, _message) -> do
                    let _xtype = MtLog

                    LSP.sendNotification SWindowLogMessage LogMessageParams{..}

                    liftIO (fail (Text.unpack _message))

                  Left (severity_, _message) -> do
                    let _xtype = case severity_ of
                          Error   -> MtError
                          Warning -> MtWarning
                          Info    -> MtInfo
                          Log     -> MtLog

                    LSP.sendNotification SWindowShowMessage ShowMessageParams{..}
                    liftIO (fail (Text.unpack _message))
                  Right a -> do
                      return a

                return (newState, result)

          backward = liftIO

  exitCode <- LSP.runServer ServerDefinition{..}

  case exitCode of
      0 -> return ()
      n -> Exit.exitWith (ExitFailure n)

-- | sets the output logger.
-- | if no filename is provided then logger is disabled, if input is string `[OUTPUT]` then log goes to stderr,
-- | which then redirects inside VSCode to the output pane of the plugin.
setupLogger :: Maybe FilePath -> IO () -- TODO: ADD verbosity
setupLogger  Nothing          = pure ()
setupLogger (Just "[OUTPUT]") = LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLogger file              = LSP.setupLogger file [] System.Log.Logger.DEBUG


-- Tells the LSP client to notify us about file changes. Handled behind the
-- scenes by haskell-lsp (in Language.Haskell.LSP.VFS); we don't handle the
-- corresponding notifications ourselves.
syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TdSyncIncremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just (InR (SaveOptions (Just False)))
  }
