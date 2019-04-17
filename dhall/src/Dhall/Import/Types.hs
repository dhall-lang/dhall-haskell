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
  , pretty
  )
import Dhall.Parser (Src)
import Dhall.TypeCheck (X)
import Lens.Family (LensLike')
import System.FilePath (isRelative, splitDirectories)
import Text.Dot (Dot, NodeId, userNode, userNodeId)

import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Data.Map      as Map
import qualified Data.Text

-- | State threaded throughout the import process
data Status m = Status
    { _stack :: NonEmpty Import
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

    , _dot :: Dot NodeId
    -- ^ Graph of all the imports visited so far

    , _nextNodeId :: Int
    -- ^ Next node id to be used for the dot graph generation

    , _cache :: Map Import (NodeId, Expr Src X)
    -- ^ Cache of imported expressions with their node id in order to avoid
    --   importing the same expression twice with different values

    , _manager :: Maybe Dynamic
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

    , _standardVersion :: StandardVersion

    , _normalizer :: Maybe (ReifiedNormalizer X)

    , _startingContext :: Context (Expr Src X)

    , _resolver :: Import -> StateT (Status m) m (Expr Src Import)

    , _cacher :: Import -> Expr Src X -> StateT (Status m) m ()
    }

-- | Default starting `Status` that is polymorphic in the base `Monad`
emptyStatusWith
    :: (Import -> StateT (Status m) m (Expr Src Import))
    -> (Import -> Expr Src X -> StateT (Status m) m ())
    -> FilePath
    -> Status m
emptyStatusWith _resolver _cacher rootDirectory = Status {..}
  where
    _stack = pure rootImport

    _dot = importNode (userNodeId 0) rootImport

    _nextNodeId = 1

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

importNode :: NodeId -> Import -> Dot NodeId
importNode nodeId i = do
    userNode
        nodeId
        [ ("label", Data.Text.unpack $ pretty i)
        , ("shape", "box")
        , ("style", "rounded")
        ]
    pure nodeId

stack :: Functor f => LensLike' f (Status m) (NonEmpty Import)
stack k s = fmap (\x -> s { _stack = x }) (k (_stack s))

dot :: Functor f => LensLike' f (Status m) (Dot NodeId)
dot k s = fmap (\x -> s { _dot = x }) (k (_dot s))

nextNodeId :: Functor f => LensLike' f (Status m) Int
nextNodeId k s = fmap (\x -> s { _nextNodeId = x }) (k (_nextNodeId s))

cache :: Functor f => LensLike' f (Status m) (Map Import (NodeId, Expr Src X))
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
    => LensLike' f (Status m) (Import -> StateT (Status m) m (Expr Src Import))
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
