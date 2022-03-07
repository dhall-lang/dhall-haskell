{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dhall.LSP.State where

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
import Data.Map.Strict                  (Map, empty)
import Data.Text                        (Text)
import Dhall.LSP.Backend.Dhall          (Cache, DhallError, emptyCache)
import Dhall.Pretty                     (CharacterSet)
import Language.LSP.Server              (LspT)

import qualified Language.LSP.Types as J

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

data ServerConfig = ServerConfig
  { chosenCharacterSet :: Maybe CharacterSet
  } deriving Show

instance Default ServerConfig where
  def = ServerConfig { chosenCharacterSet = Nothing }

-- We need to derive the FromJSON instance manually in order to provide defaults
-- for absent fields.
instance FromJSON ServerConfig where
  parseJSON = withObject "settings" $ \v -> do
    s <- v .: "vscode-dhall-lsp-server"
    flip (withObject "vscode-dhall-lsp-server") s $ \o -> ServerConfig
      <$> o .:? "character-set" .!= Nothing

data ServerState = ServerState
  { _importCache :: Cache  -- ^ The dhall import cache
  , _errors :: Map J.Uri DhallError  -- ^ Map from dhall files to their errors
  , _httpManager :: Maybe Dynamic
  -- ^ The http manager used by dhall's import infrastructure
  }

makeLenses ''ServerState

initialState :: ServerState
initialState = ServerState {..}
  where
    _importCache = emptyCache
    _errors = empty
    _httpManager = Nothing
