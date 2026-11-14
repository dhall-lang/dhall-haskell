{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This is the entry point for the LSP server. -}
module Dhall.LSP.Server (
      run
    , runWith
    ) where

import Colog.Core                    (LogAction, WithSeverity)
import Control.Monad.IO.Class        (liftIO)
import Data.Aeson                    (fromJSON)
import Data.Default
import Dhall                         (EvaluateSettings, defaultEvaluateSettings)
import Dhall.LSP.Backend.Dhall       (emptyCache)
import Dhall.LSP.Features            (featureHandlers)
import Dhall.LSP.Handlers
    ( cancelationHandler
    , completionHandler
    , didOpenTextDocumentNotificationHandler
    , didSaveTextDocumentNotificationHandler
    , documentDidCloseHandler
    , documentFormattingHandler
    , documentLinkHandler
    , executeCommandHandler
    , hoverHandler
    , initializedHandler
    , setTraceHandler
    , textDocumentChangeHandler
    , workspaceChangeConfigurationHandler
    )
import Dhall.LSP.State
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
    ( LspServerLog
    , Options (..)
    , ServerDefinition (..)
    , type (<~>) (..)
    )
import Prettyprinter                 (Doc, Pretty, pretty, viaShow)
import System.Exit                   (ExitCode (..))
import System.IO                     (stdin, stdout)

import qualified Colog.Core                       as Colog
import qualified Control.Concurrent.MVar          as MVar
import qualified Control.Exception                as Exception
import qualified Control.Monad.Trans.Except       as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson                       as Aeson
import qualified Data.IORef                       as IORef
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as Text
import qualified Language.LSP.Logging             as LSP
import qualified Language.LSP.Server              as LSP
import qualified System.Exit                      as Exit

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run = runWith defaultEvaluateSettings

-- | The main entry point for the LSP server.
runWith :: EvaluateSettings -> Maybe FilePath -> IO ()
runWith settings = withLogger $ \ioLogger -> do
  let clientLogger = Colog.cmap (fmap (Text.pack . show . pretty)) LSP.defaultClientLogger

  let lspLogger = clientLogger <> Colog.hoistLogAction liftIO ioLogger

  errorStore <- IORef.newIORef Map.empty
  documentStore <- IORef.newIORef Map.empty
  envRef <- IORef.newIORef Nothing
  negative <- IORef.newIORef Map.empty
  bodies <- IORef.newIORef Map.empty
  chains <- IORef.newIORef Map.empty
  origins <- IORef.newIORef Map.empty
  importCache <- IORef.newIORef emptyCache
  state <-
      MVar.newMVar
          (initialState importCache errorStore documentStore envRef negative bodies chains origins)

  let defaultConfig = def

#if MIN_VERSION_lsp(2,2,0)
  let configSection = "dhall"

  let onConfigChange _newConfig = return ()

  let parseConfig = parseServerConfig
#else
  let onConfigurationChange = parseServerConfig
#endif

  let doInitialize environment _request = do
          return (Right environment)

  let options = def
        { LSP.optTextDocumentSync = Just syncOptions

        , optCompletionTriggerCharacters = Just [':', '.', '/']

        -- Note that this registers the dhall.server.lint command
        -- with VSCode, which means that our plugin can't expose a
        -- command of the same name. In the case of dhall.lint we
        -- name the server-side command dhall.server.lint to work
        -- around this peculiarity.
        , optExecuteCommandCommands =
            Just
              [ "dhall.server.lint",
                "dhall.server.annotateLet",
                "dhall.server.freezeImport",
                "dhall.server.freezeAllImports",
                "dhall.server.unfreezeImport",
                "dhall.server.unfreezeAllImports",
                "dhall.server.checkImportHash",
                "dhall.server.explain",
                "dhall.server.normalize",
                "dhall.server.showOriginalSource"
              ]
        }

  let staticHandlers _clientCapabilities =
        mconcat
          [ hoverHandler settings
          , didOpenTextDocumentNotificationHandler settings
          , didSaveTextDocumentNotificationHandler settings
          , executeCommandHandler settings
          , documentFormattingHandler
          , documentLinkHandler
          , completionHandler settings
          , initializedHandler
          , setTraceHandler
          , workspaceChangeConfigurationHandler
          , textDocumentChangeHandler settings
          , cancelationHandler
          , documentDidCloseHandler
          , featureHandlers settings
          ]

  let interpretHandler environment = Iso{..}
        where
          forward :: HandlerM a -> IO a
          forward handler = do
            IORef.writeIORef envRef (Just environment)
            -- Take a snapshot and release the lock before the handler runs,
            -- so one request does not block the others. The document store
            -- is an IORef shared by every snapshot.
            oldState <- MVar.readMVar state
            outcome <- Exception.try $ LSP.runLspT environment do
                (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                result <- case e of
                  Left (Log, _message) -> do
                    let _type_ = MessageType_Log

                    LSP.sendNotification SMethod_WindowLogMessage LogMessageParams{..}

                    liftIO (fail (Text.unpack _message))

                  Left (severity_, _message) -> do
                    let _type_ = case severity_ of
                          Error   -> MessageType_Error
                          Warning -> MessageType_Warning
                          Info    -> MessageType_Info
#if !MIN_TOOL_VERSION_ghc(9,2,0)
                          Log     -> MessageType_Log
#endif

                    LSP.sendNotification SMethod_WindowShowMessage ShowMessageParams{..}
                    liftIO (fail (Text.unpack _message))
                  Right a -> do
                      return a

                return (newState, result)
            case outcome of
              Left (err :: Exception.SomeException) ->
                Exception.throwIO err
              Right (newState, result) -> do
                MVar.modifyMVar_ state (\_ -> return newState)
                return result

          backward = liftIO

  exitCode <- LSP.runServerWithHandles ioLogger lspLogger stdin stdout ServerDefinition{..}

  case exitCode of
      0 -> return ()
      n -> Exit.exitWith (ExitFailure n)

-- | Parse the settings object sent by the client.  A JSON null means the
--   client has no settings for this section (typical while the editor starts
--   up); keep the old configuration in that case instead of failing.
parseServerConfig :: ServerConfig -> Aeson.Value -> Either Text.Text ServerConfig
parseServerConfig oldConfig Aeson.Null = Right oldConfig
parseServerConfig _ json =
    case fromJSON json of
        Aeson.Success config -> Right config
        Aeson.Error   string ->
            Left
                (  "expected a settings object with an optional \"vscode-dhall-lsp-server\" section, "
                <> "for example {\"vscode-dhall-lsp-server\": {\"character-set\": \"ascii\"}}; "
                <> Text.pack string
                )

-- | Retrieve the output logger.
-- If no filename is provided then logger is disabled, if input is the string
-- `[OUTPUT]` then we log to stderr.
-- TODO: ADD verbosity
withLogger :: (LogAction IO (WithSeverity LspServerLog) -> IO ()) -> Maybe FilePath -> IO ()
withLogger k = \case
  Nothing -> k (Colog.LogAction (const (pure ())))
  Just "[OUTPUT]" -> k' Colog.logStringStderr
  Just fp -> Colog.withLogStringFile fp k'
  where
    k' = k . Colog.cmap (show . prettyMsg)

    prettyMsg :: Pretty a => WithSeverity a -> Doc ann
    prettyMsg l = "[" <> viaShow (Colog.getSeverity l) <> "] " <> pretty (Colog.getMsg l)

-- Tells the LSP client to notify us about file changes. Handled behind the
-- scenes by haskell-lsp (in Language.Haskell.LSP.VFS); we don't handle the
-- corresponding notifications ourselves.
syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TextDocumentSyncKind_Incremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just (InR (SaveOptions (Just False)))
  }
