{-# LANGUAGE TemplateHaskell #-}
module Dhall.LSP.State where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as J

import Control.Lens.TH (makeLenses)
import Lens.Family (LensLike')
import Data.Map.Strict (Map, empty)
import Data.Dynamic (Dynamic)
import Dhall.LSP.Backend.Dhall (DhallError, Cache, emptyCache)
import Data.Text (Text)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)

-- Inside a handler we have access to the ServerState. The exception layer
-- allows us to fail gracefully, displaying a message to the user via the
-- "ShowMessage" mechanism of the lsp standard.
type HandlerM = ExceptT (Severity, Text) (StateT ServerState IO)

data Severity = Error
              -- ^ Error displayed to the user.
              | Warning
              -- ^ Warning displayed to the user.
              | Info
              -- ^ Information displayed to the user.
              | Log
              -- ^ Log message, not displayed by default.

data ServerState = ServerState
  { _importCache :: Cache  -- ^ The dhall import cache
  , _errors :: Map J.Uri DhallError  -- ^ Map from dhall files to their errors
  , _httpManager :: Maybe Dynamic
  -- ^ The http manager used by dhall's import infrastructure
  , _lspFuncs :: LSP.LspFuncs ()
  -- ^ Access to the lsp functions supplied by haskell-lsp
  }

makeLenses ''ServerState

sendFunc :: Functor f =>
  LensLike' f (LSP.LspFuncs ()) (LSP.FromServerMessage -> IO ())
sendFunc k s = fmap (\x -> s {LSP.sendFunc = x}) (k (LSP.sendFunc s))

initialState :: LSP.LspFuncs () -> ServerState
initialState lsp = ServerState {..}
  where
    _importCache = emptyCache
    _errors = empty
    _httpManager = Nothing
    _lspFuncs = lsp
