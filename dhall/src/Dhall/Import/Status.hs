{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Import.Status where

import Control.Monad.Trans.State.Strict (StateT)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Void (Void)
import Dhall.Context (Context)
import Dhall.Core
  ( Directory (..)
  , Expr
  , File (..)
  , FilePrefix (..)
  , Import (..)
  , ImportHashed (..)
  , ImportMode (..)
  , ImportType (..)
  , ReifiedNormalizer(..)
  , URL
  )
import Dhall.Import.Types
import Dhall.Src (Src)
import Lens.Family (LensLike')
import System.FilePath (isRelative, splitDirectories)

import qualified Dhall.Context
import qualified Data.Map      as Map
import qualified Data.Text

-- | An import that has been fully interpreted
data ImportSemantics = ImportSemantics
    { importSemantics :: Expr Src Void
    -- ^ The fully resolved import, typechecked and beta-normal.
    }

-- | State threaded throughout the import process
data Status = Status
    { _stack :: NonEmpty Chained
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

    , _graph :: [Depends]
    -- ^ Graph of all the imports visited so far, represented by a list of
    --   import dependencies.

    , _cache :: Map Chained ImportSemantics
    -- ^ Cache of imported expressions with their node id in order to avoid
    --   importing the same expression twice with different values

    , _remote :: URL -> StateT Status IO Data.Text.Text
    -- ^ The remote resolver, fetches the content at the given URL.

    , _normalizer :: Maybe (ReifiedNormalizer Void)

    , _startingContext :: Context (Expr Src Void)

    , _semanticCacheMode :: SemanticCacheMode
    }

-- | Initial `Status`, parameterised over the remote resolver, importing
--   relative to the given directory.
emptyStatusWith :: (URL -> StateT Status IO Data.Text.Text) -> FilePath -> Status
emptyStatusWith _remote rootDirectory = Status {..}
  where
    _stack = pure (Chained rootImport)

    _graph = []

    _cache = Map.empty

    _normalizer = Nothing

    _startingContext = Dhall.Context.empty

    _semanticCacheMode = UseSemanticCache

    prefix = if isRelative rootDirectory
      then Here
      else Absolute
    pathComponents =
        fmap Data.Text.pack (reverse (splitDirectories rootDirectory))

    dirAsFile = File (Directory pathComponents) "."

    -- Fake import to set the directory we're relative to.
    rootImport = Import
      { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local prefix dirAsFile
        }
      , importMode = Code
      }

-- | Lens from a `Status` to its `_stack` field
stack :: Functor f => LensLike' f Status (NonEmpty Chained)
stack k s = fmap (\x -> s { _stack = x }) (k (_stack s))

-- | Lens from a `Status` to its `_graph` field
graph :: Functor f => LensLike' f Status [Depends]
graph k s = fmap (\x -> s { _graph = x }) (k (_graph s))

-- | Lens from a `Status` to its `_cache` field
cache :: Functor f => LensLike' f Status (Map Chained ImportSemantics)
cache k s = fmap (\x -> s { _cache = x }) (k (_cache s))

-- | Lens from a `Status` to its `_remote` field
remote
    :: Functor f => LensLike' f Status (URL -> StateT Status IO Data.Text.Text)
remote k s = fmap (\x -> s { _remote = x }) (k (_remote s))

-- | Lens from a `Status` to its `_normalizer` field
normalizer :: Functor f => LensLike' f Status (Maybe (ReifiedNormalizer Void))
normalizer k s = fmap (\x -> s {_normalizer = x}) (k (_normalizer s))

-- | Lens from a `Status` to its `_startingContext` field
startingContext :: Functor f => LensLike' f Status (Context (Expr Src Void))
startingContext k s =
    fmap (\x -> s { _startingContext = x }) (k (_startingContext s))
