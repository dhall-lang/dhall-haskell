{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Dhall.Context where

import Control.Monad.Reader
import Crypto.Hash (SHA256, Digest)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Dhall.Binary
import Dhall.Core
import Dhall.Eval
import Network.HTTP.Client (Manager)
import Data.List.NonEmpty
import System.FilePath

import qualified Data.Map.Strict as Map
import qualified Data.Text


-- | An import which has been previously resolved during elaboration. The
--   contained 'Val' is the lazy value of the imported expression.
data ResolvedImport = ResolvedImport !CoreImport Val

-- | Types of local bindings.
data Types = TEmpty | TBind !Types {-# unpack #-} !Text Val

typesNames :: Types -> Names
typesNames TEmpty         = NEmpty
typesNames (TBind ts x _) = NBind (typesNames ts) x

-- | Normal types of local bindings.
typesToList :: Types -> [(Text, Nf)]
typesToList TEmpty         = []
typesToList (TBind ts x v) = (x, quote (typesNames ts) v): typesToList ts

data Cxt = Cxt {
    _values :: !Env
  , _types  :: !Types
  }

emptyCxt :: Cxt
emptyCxt = Cxt Empty TEmpty

quoteCxt :: Cxt -> Val -> Nf
quoteCxt Cxt{..} = quote (envNames _values)
{-# INLINE quoteCxt #-}

quoteCxtCore :: Cxt -> Val -> Core
quoteCxtCore cxt = nfToCore . quoteCxt cxt
{-# INLINE quoteCxtCore #-}

define :: Text -> Val -> Val -> Cxt -> Cxt
define x t a (Cxt ts as) = Cxt (Extend ts x t) (TBind as x a)
{-# INLINE define #-}

bind :: Text -> Val -> Cxt -> Cxt
bind x a (Cxt ts as) = Cxt (Skip ts x) (TBind as x a)
{-# INLINE bind #-}

data ImportOptions
    -- ^ Imports are disabled.
  = ImportsDisabled
    -- ^ Freeze the remote imports in the root expression.
  | FreezeRemote
    -- ^ Free all imports in the root expression.
  | FreezeAll
    -- ^ Resolve imports without freezing.
  | NoFreezing
    -- ^ Resolve, normalize and unfold every import in the root expression.
  | OldResolve
  deriving Show

data CacheEntry = CacheEntry {
    _core    :: !Core
  , _value   :: Val
  , _type    :: VType
  , _nf      :: Nf
  , _hash    :: Digest SHA256
  }

data ImportState = ImportState {
    _stack :: !(NonEmpty CoreImport)
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

  , _cache :: !(IORef (Map CoreImport CacheEntry))
    -- ^ Cache of imported expressions in order to avoid importing the same
    --   expression twice with different values

  , _manager :: !(IORef (Maybe Manager))
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

  , _standardVersion :: !StandardVersion

    -- ^ Options for imports.
  , _importOptions :: !ImportOptions

    -- ^ Import graph.
  , _graph :: !(IORef (Map CoreImport (Int, Set CoreImport)))
  }

type ElabM = ReaderT ImportState IO

rootImport :: FilePath -> CoreImport
rootImport rootDir =
  let prefix = if isRelative rootDir then Here else Absolute
      pathComponents =
        Data.Text.pack <$> Prelude.reverse (splitDirectories rootDir)
      dirAsFile = File (Directory pathComponents) "."
  in Import
       (ImportHashed
         Nothing
         (Local prefix dirAsFile))
       Code

rootState :: FilePath -> IO ImportState
rootState rootDir = do
  let root             = rootImport rootDir
      _stack           = pure root
      _standardVersion = defaultStandardVersion
      _importOptions   = NoFreezing
  _cache   <- newIORef mempty
  _manager <- newIORef Nothing
  _graph   <- newIORef (Map.singleton root (0, mempty))
  pure ImportState{..}
