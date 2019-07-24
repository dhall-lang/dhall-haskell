{-# LANGUAGE RecordWildCards, OverloadedStrings, BangPatterns #-}

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

-- | Types of local bindings.
data Types = TEmpty | TBind !Types Val

-- | Normal types of in-scope bindings.
typesToList :: Types -> [Expr X]
typesToList = fst . go where
  go :: Types -> ([Expr X], Int)
  go TEmpty       = ([], 0)
  go (TBind ts v) = case go ts of
    (ts', l) -> let !l' = l + 1 in (quote NoAlpha l v : ts', l')

data Cxt = Cxt {
    _values :: !Env   -- ^ Values of bindings.
  , _types  :: !Types -- ^ Types of bindings.
  , _names  :: !Names -- ^ Names of binders.
  , _lvl    :: !Int   -- ^ Size of the context.
  }

emptyCxt :: Cxt
emptyCxt = Cxt Empty TEmpty NEmpty 0

quoteCxt :: Cxt -> Val -> Expr X
quoteCxt Cxt{..} = quote NoAlpha _lvl
{-# INLINE quoteCxt #-}

-- quoteCxtCore :: Cxt -> Val -> Core
-- quoteCxtCore cxt = nfToCore . quoteCxt cxt
-- {-# INLINE quoteCxtCore #-}

-- | Extend context with a definition.
define :: Text -> Val -> Val -> Cxt -> Cxt
define x t a (Cxt vs as ns l) = Cxt (Extend vs t) (TBind as a) (NBind ns x) (l + 1)
{-# INLINE define #-}

-- | Extend context with a bound name.
bind :: Text -> Val -> Cxt -> Cxt
bind x a (Cxt ts as ns l) = Cxt (Skip ts) (TBind as a) (NBind ns x) (l + 1)
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

-- | Entry for imports cache.
data CacheEntry = CacheEntry {
    _core    :: !(Expr I)
  , _value   :: Val
  , _type    :: Val
  , _nf      :: Expr X
  , _hash    :: Digest SHA256
  }

data ImportState = ImportState {
    _stack :: !(NonEmpty (Import I))
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

  , _cache :: !(IORef (Map (Import I) CacheEntry))
    -- ^ Cache of imported expressions in order to avoid importing the same
    --   expression twice with different values

  , _manager :: !(IORef (Maybe Manager))
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

  , _standardVersion :: !StandardVersion

    -- ^ Options for imports.
  , _importOptions :: !ImportOptions

    -- ^ Import graph.
  , _graph :: !(IORef (Map (Import I) (Int, Set (Import I))))
  }

type ElabM = ReaderT ImportState IO

rootImport :: FilePath -> Import I
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
