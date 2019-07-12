{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}

module Dhall.Import.Types where

import Control.Exception (Exception)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Dynamic
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Dhall.Binary (StandardVersion(..))
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
  )
import Dhall.Parser (Src)
import Dhall.TypeCheck (X)
import Lens.Family (LensLike')
import System.FilePath (isRelative, splitDirectories)

import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Data.Graph    as Graph
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Text

data Resolved = Resolved
    { resolvedExpression :: Expr Src Import
    -- ^ Expression with its immediate imports resolved
    , newImport          :: Import
    -- ^ New import to use in place of the original import for chaining
    --   downstream imports
    }

-- | `parent` imports (i.e. depends on) `child`
data Depends = Depends { parent :: Import, child :: Import }

-- | State threaded throughout the import process
data Status m = Status
    { _stack :: NonEmpty Import
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

<<<<<<< HEAD
    , _graph :: [(Import,Import)]
    -- ^ Graph of all the imports visited so far, represented by a list of
    --   edges. An edge `(a, b)` means that `b` imported `a`.
=======
    , _graph :: [Depends]
    -- ^ Graph of all the imports visited so far, represented by a list of
    --   import dependencies.
>>>>>>> master

    , _cache :: Map Import (Expr Src X)
    -- ^ Cache of imported expressions with their node id in order to avoid
    --   importing the same expression twice with different values

    , _manager :: Maybe Dynamic
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

    , _standardVersion :: StandardVersion

    , _normalizer :: Maybe (ReifiedNormalizer X)

    , _startingContext :: Context (Expr Src X)

    , _resolver :: Import -> StateT (Status m) m Resolved

    , _cacher :: Import -> Expr Src X -> StateT (Status m) m ()
    }

-- | Default starting `Status` that is polymorphic in the base `Monad`
emptyStatusWith
    :: (Import -> StateT (Status m) m Resolved)
    -> (Import -> Expr Src X -> StateT (Status m) m ())
    -> FilePath
    -> Status m
emptyStatusWith _resolver _cacher rootDirectory = Status {..}
  where
    _stack = pure rootImport

    _graph = []

    _cache = Map.empty

    _manager = Nothing

    _standardVersion = Dhall.Binary.defaultStandardVersion

    _normalizer = Nothing

    _startingContext = Dhall.Context.empty

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

stack :: Functor f => LensLike' f (Status m) (NonEmpty Import)
stack k s = fmap (\x -> s { _stack = x }) (k (_stack s))

<<<<<<< HEAD
graph :: Functor f => LensLike' f (Status m) [(Import, Import)]
=======
graph :: Functor f => LensLike' f (Status m) [Depends]
>>>>>>> master
graph k s = fmap (\x -> s { _graph = x }) (k (_graph s))

cache :: Functor f => LensLike' f (Status m) (Map Import (Expr Src X))
cache k s = fmap (\x -> s { _cache = x }) (k (_cache s))

manager :: Functor f => LensLike' f (Status m) (Maybe Dynamic)
manager k s = fmap (\x -> s { _manager = x }) (k (_manager s))

standardVersion :: Functor f => LensLike' f (Status m) StandardVersion
standardVersion k s =
    fmap (\x -> s { _standardVersion = x }) (k (_standardVersion s))

normalizer :: Functor f => LensLike' f (Status m) (Maybe (ReifiedNormalizer X))
normalizer k s = fmap (\x -> s {_normalizer = x}) (k (_normalizer s))

startingContext :: Functor f => LensLike' f (Status m) (Context (Expr Src X))
startingContext k s =
    fmap (\x -> s { _startingContext = x }) (k (_startingContext s))

resolver
    :: Functor f
    => LensLike' f (Status m) (Import -> StateT (Status m) m Resolved)
resolver k s = fmap (\x -> s { _resolver = x }) (k (_resolver s))

cacher
    :: Functor f
    => LensLike' f (Status m) (Import -> Expr Src X -> StateT (Status m) m ())
cacher k s = fmap (\x -> s { _cacher = x }) (k (_cacher s))

{-| This exception indicates that there was an internal error in Dhall's
    import-related logic
    the `expected` type then the `extract` function must succeed.  If not, then
    this exception is thrown

    This exception indicates that an invalid `Type` was provided to the `input`
    function
-}
data InternalError = InternalError deriving (Typeable)


instance Show InternalError where
    show InternalError = unlines
        [ _ERROR <> ": Compiler bug                                                        "
        , "                                                                                "
        , "Explanation: This error message means that there is a bug in the Dhall compiler."
        , "You didn't do anything wrong, but if you would like to see this problem fixed   "
        , "then you should report the bug at:                                              "
        , "                                                                                "
        , "https://github.com/dhall-lang/dhall-haskell/issues                              "
        , "                                                                                "
        , "Please include the following text in your bug report:                           "
        , "                                                                                "
        , "```                                                                             "
        , "Header extraction failed even though the header type-checked                    "
        , "```                                                                             "
        ]
      where
        _ERROR :: String
        _ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception InternalError

-- | Wrapper around `HttpException`s with a prettier `Show` instance.
--
-- In order to keep the library API constant even when the @with-http@ Cabal
-- flag is disabled the pretty error message is pre-rendered and the real
-- 'HttpExcepion' is stored in a 'Dynamic'
data PrettyHttpException = PrettyHttpException String Dynamic
    deriving (Typeable)

instance Exception PrettyHttpException

instance Show PrettyHttpException where
  show (PrettyHttpException msg _) = msg

-- | Given a list of imports whose underlying files have changed, remove them
--  from the import cache. Including any "reverse dependencies" is up to the
--  caller!
invalidate :: [Import] -> Status m -> Status m
invalidate imports Status {..} =
    Status {_cache = _cache', _graph = _graph', ..}
  where
    invalid = Set.fromList imports
    _cache' = Map.filterWithKey (\i _ -> Set.notMember i invalid) _cache
    _graph' = filter (\(from, to) -> Set.notMember from invalid
                                       && Set.notMember to invalid) _graph

-- | Calculate the reverse dependencies of an import in the import cache. Does
--   not include the import itself, assuming there aren't any cycles in the
--   import graph!
reverseDependencies :: Import -> Status m -> [Import]
reverseDependencies import_ Status {..} =
    map (\(i,_,_) -> i) . map importFromVertex . concat $
        do vertex <- vertexFromImport import_
           return (Graph.reachable importGraph vertex)
  where
    imports = map fst _graph ++ map snd _graph

    -- adjacency lists representing the transposed dependency graph
    adjacencyLists = foldr
                       (\(from, to) -> Map.adjust (from :) to)
                       (Map.fromList [ (i,[]) | i <- imports])
                       _graph

    -- representation using Data.Graph
    (importGraph, importFromVertex, vertexFromImport) =
        Graph.graphFromEdges
            [(node, node, neighbours)
            | (node, neighbours) <- Map.assocs adjacencyLists]
