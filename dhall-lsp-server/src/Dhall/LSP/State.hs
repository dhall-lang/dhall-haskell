{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dhall.LSP.State where

import Control.Exception                 (SomeException)
import Control.Lens                     (use)
import Control.Lens.TH                  (makeLenses)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.State.Class        (MonadState)
import Control.Monad.Trans.Except       (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson
    ( FromJSON (..)
    , Value (..)
    , withObject
    , (.!=)
    , (.:?)
    )
import Data.Default                     (Default (def))
import Data.Dynamic                     (Dynamic)
import Data.IORef                       (IORef, modifyIORef', readIORef, writeIORef)
import Data.Map.Strict                  (Map)
import Data.Text                        (Text)
import Data.Time.Clock                  (UTCTime)
import Data.Void                        (Void)
import Dhall.Core                       (Expr)
import Dhall.Import                    (Chained, CollectedImportError)
import Dhall.LSP.Backend.Dhall          (Cache, DhallError)
import Dhall.Parser                     (Src)
import Dhall.Pretty                     (ChooseCharacterSet(..))
import Dhall.TypeCheck                  (TypingContext)
import Language.LSP.Server              (LanguageContextEnv, LspT)

import qualified Language.LSP.Protocol.Types as J

-- Inside a handler we have access to the ServerState. The exception layer
-- allows us to fail gracefully, displaying a message to the user via the
-- "ShowMessage" mechanism of the lsp standard.
type HandlerM =
    ExceptT (Severity, Text) (StateT ServerState (LspT ServerConfig IO))

data Severity = Error
              -- ^ Error displayed to the user.
              | Warning
              -- ^ Warning displayed to the user.
              | Info
              -- ^ Information displayed to the user.
              | Log
              -- ^ Log message, not displayed by default.

-- | How much of a normal form the server will render, in bytes.
--   16KiB fills a screen and leaves the rest unrendered.
defaultOutputBytes :: Int
defaultOutputBytes = 16 * 1024

data ServerConfig = ServerConfig
  { chosenCharacterSet :: ChooseCharacterSet
  , maxOutputSize :: Int
  } deriving (Eq, Show)

instance Default ServerConfig where
  def = ServerConfig
    { chosenCharacterSet = AutoInferCharSet
    , maxOutputSize = defaultOutputBytes
    }

-- We need to derive the FromJSON instance manually in order to provide defaults
-- for absent fields.  JSON null and a missing "vscode-dhall-lsp-server"
-- section both mean "no settings": use the defaults.
instance FromJSON ServerConfig where
  parseJSON Null = pure def
  parseJSON value = flip (withObject "settings") value $ \v -> do
    mSection <- v .:? "vscode-dhall-lsp-server"
    case mSection of
      Nothing -> pure def
      Just Null -> pure def
      Just section ->
        flip (withObject "vscode-dhall-lsp-server") section $ \o -> ServerConfig
          <$> o .:? "character-set" .!= AutoInferCharSet
          <*> o .:? "maxOutputSize" .!= defaultOutputBytes

data ServerState = ServerState
  { _importCache :: IORef Cache
  -- ^ Shared import cache.  An IORef so background diagnostics and request
  --   handlers see the same graph after @textDocument/didChange@.
  , _errors :: IORef (Map J.Uri DocErrors)
  -- ^ Map from dhall files to their errors.  An IORef because didChange
  --   analysis runs in a background thread on a snapshot of the state; the
  --   fresh diagnostics must still reach the handlers.
  , _httpManager :: Maybe Dynamic
  -- ^ The http manager used by dhall's import infrastructure
  , _documents :: IORef (Map J.Uri DocSnap)
  -- ^ Per-document analysis. Shared across handler snapshots.
  , _lspEnv :: IORef (Maybe (LanguageContextEnv ServerConfig))
  -- ^ Captured so background analysis can publish diagnostics.
  , _negativeImports :: IORef (Map Text (UTCTime, SomeException))
  -- ^ Remote imports that failed recently.  Retried after 30 seconds.
  , _importBodies :: IORef (Map Text Text)
  -- ^ Source text fetched while typechecking imports.
  , _importChains :: IORef (Map Text Chained)
  -- ^ Chained import for each key in 'importBodies'.
  , _mirrorOrigins :: IORef (Map FilePath Chained)
  -- ^ Cache file or 'dhall-import:' name of a mirror, and the import it came from.
  }

-- | The errors of one open document, split by analysis stage.  Parse errors
--   describe the current text.  Import and type errors were computed from
--   'errSemanticText', the most recent text that parsed, so a later parse
--   error can keep the ones whose source slice is unchanged.
data DocErrors = DocErrors
  { errParse :: [DhallError]
  , errImports :: [CollectedImportError]
  , errTypes :: [DhallError]
  , errSemanticText :: Text
  }

-- | The last analysis of one open document.
data DocSnap = DocSnap
  { snapVersion :: !Int
  , snapGeneration :: !Int
  , snapText :: Text
  , snapLastGood :: Maybe Text
  -- ^ Text of the last analysis that parsed.  Navigation uses it when the
  --   buffer has a syntax error.
  --
  --   Binder names, denoted values, and the context after each top-level let.
  --   A later edit reuses a prefix only while both the name and the value
  --   still match.
  , snapPrefixNames :: [Text]
  , snapPrefixValueKeys :: [Text]
  , snapPrefixValues :: [Expr Void Void]
  , snapPrefixContexts :: [TypingContext Src]
  , snapResolved :: Maybe (Expr Src Void)
  }

makeLenses ''ServerState

readImportCache :: (MonadIO m, MonadState ServerState m) => m Cache
readImportCache = do
    ref <- use importCache
    liftIO (readIORef ref)

writeImportCache :: (MonadIO m, MonadState ServerState m) => Cache -> m ()
writeImportCache cache' = do
    ref <- use importCache
    liftIO (writeIORef ref cache')

modifyImportCache :: (MonadIO m, MonadState ServerState m) => (Cache -> Cache) -> m ()
modifyImportCache f = do
    ref <- use importCache
    liftIO (modifyIORef' ref f)

initialState
    :: IORef Cache
    -> IORef (Map J.Uri DocErrors)
    -> IORef (Map J.Uri DocSnap)
    -> IORef (Maybe (LanguageContextEnv ServerConfig))
    -> IORef (Map Text (UTCTime, SomeException))
    -> IORef (Map Text Text)
    -> IORef (Map Text Chained)
    -> IORef (Map FilePath Chained)
    -> ServerState
initialState
    importCacheRef
    errorsRef
    documentsRef
    lspEnvRef
    negativeImportsRef
    importBodiesRef
    importChainsRef
    mirrorOriginsRef =
    ServerState
        { _importCache = importCacheRef
        , _errors = errorsRef
        , _documents = documentsRef
        , _lspEnv = lspEnvRef
        , _negativeImports = negativeImportsRef
        , _importBodies = importBodiesRef
        , _importChains = importChainsRef
        , _mirrorOrigins = mirrorOriginsRef
        , _httpManager = Nothing
        }
