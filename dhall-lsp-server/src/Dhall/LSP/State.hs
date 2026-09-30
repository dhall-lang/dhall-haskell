{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dhall.LSP.State where

import Control.Exception                 (SomeException)
import Control.Lens.TH                  (makeLenses)
import Control.Monad.Trans.Except       (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson
    ( FromJSON (..)
    , withObject
    , (.!=)
    , (.:)
    , (.:?)
    )
import Data.Default                     (Default (def))
import Data.Dynamic                     (Dynamic)
import Data.IORef                       (IORef)
import Data.Map.Strict                  (Map, empty)
import Data.Text                        (Text)
import Data.Time.Clock                  (UTCTime)
import Data.Void                        (Void)
import Dhall.Core                       (Expr)
import Dhall.LSP.Backend.Dhall          (Cache, DhallError, emptyCache)
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
  } deriving Show

instance Default ServerConfig where
  def = ServerConfig
    { chosenCharacterSet = AutoInferCharSet
    , maxOutputSize = defaultOutputBytes
    }

-- We need to derive the FromJSON instance manually in order to provide defaults
-- for absent fields.
instance FromJSON ServerConfig where
  parseJSON = withObject "settings" $ \v -> do
    s <- v .: "vscode-dhall-lsp-server"
    flip (withObject "vscode-dhall-lsp-server") s $ \o -> ServerConfig
      <$> o .:? "character-set" .!= AutoInferCharSet
      <*> o .:? "maxOutputSize" .!= defaultOutputBytes

data ServerState = ServerState
  { _importCache :: Cache  -- ^ The dhall import cache
  , _errors :: Map J.Uri DhallError  -- ^ Map from dhall files to their errors
  , _httpManager :: Maybe Dynamic
  -- ^ The http manager used by dhall's import infrastructure
  , _documents :: IORef (Map J.Uri DocSnap)
  -- ^ Per-document analysis. Shared across handler snapshots.
  , _lspEnv :: IORef (Maybe (LanguageContextEnv ServerConfig))
  -- ^ Captured so background analysis can publish diagnostics.
  , _negativeImports :: IORef (Map Text (UTCTime, SomeException))
  -- ^ Remote imports that failed recently.  Retried after 30 seconds.
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
  , snapPrefixValues :: [Expr Void Void]
  , snapPrefixContexts :: [TypingContext Src]
  }

makeLenses ''ServerState

initialState
    :: IORef (Map J.Uri DocSnap)
    -> IORef (Maybe (LanguageContextEnv ServerConfig))
    -> IORef (Map Text (UTCTime, SomeException))
    -> ServerState
initialState _documents _lspEnv _negativeImports = ServerState {..}
  where
    _importCache = emptyCache
    _errors = empty
    _httpManager = Nothing
