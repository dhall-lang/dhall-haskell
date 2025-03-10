{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wall #-}

module Dhall.Import.Types where

import Control.Exception                (Exception)
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString                  (ByteString)
import Data.CaseInsensitive             (CI)
import Data.Dynamic
import Data.HashMap.Strict              (HashMap)
import Data.List.NonEmpty               (NonEmpty)
import Data.Void                        (Void)
import Dhall.Context                    (Context)
import Dhall.Core
    ( Expr
    , Import (..)
    , ReifiedNormalizer (..)
    , URL
    )
import Dhall.Map                        (Map)
import Dhall.Parser                     (Src)
import Lens.Micro                       (Lens', lens)
import Prettyprinter                    (Pretty (..))

#ifdef WITH_HTTP
import qualified Dhall.Import.Manager
#endif

import qualified Data.Text
import qualified Dhall.Context
import qualified Dhall.Map          as Map
import qualified Dhall.Substitution

-- | A fully \"chained\" import, i.e. if it contains a relative path that path
--   is relative to the current directory. If it is a remote import with headers
--   those are well-typed (either of type `List { header : Text, value Text}` or
--   `List { mapKey : Text, mapValue Text})` and in normal form. These
--   invariants are preserved by the API exposed by @Dhall.Import@.
newtype Chained = Chained
    { chainedImport :: Import
      -- ^ The underlying import
    }
  deriving (Eq, Ord)

instance Pretty Chained where
    pretty (Chained import_) = pretty import_

-- | An import that has been fully interpeted
newtype ImportSemantics = ImportSemantics
    { importSemantics :: Expr Void Void
    -- ^ The fully resolved import, typechecked and beta-normal.
    }

-- | `parent` imports (i.e. depends on) `child`
data Depends = Depends { parent :: Chained, child :: Chained }

{-| This enables or disables the semantic cache for imports protected by
    integrity checks
-}
data SemanticCacheMode = IgnoreSemanticCache | UseSemanticCache deriving (Eq)

-- | Shared state for HTTP requests
type Manager =
#ifdef WITH_HTTP
    Dhall.Import.Manager.Manager
#else
    ()
#endif

-- | The default HTTP 'Manager'
defaultNewManager :: IO Manager
defaultNewManager =
#ifdef WITH_HTTP
  Dhall.Import.Manager.defaultNewManager
#else
  pure ()
#endif

-- | HTTP headers
type HTTPHeader = (CI ByteString, ByteString)

-- | A map of site origin -> HTTP headers
type OriginHeaders = HashMap Data.Text.Text [HTTPHeader]

{-| Used internally to track whether or not we've already warned the user about
    caching issues
-}
data CacheWarning = CacheNotWarned | CacheWarned

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

    , _newManager :: IO Manager
    , _manager :: Maybe Manager
    -- ^ Used to cache the `Dhall.Import.Manager.Manager` when making multiple
    -- requests

    , _loadOriginHeaders :: StateT Status IO OriginHeaders
    -- ^ Load the origin headers from environment or configuration file.
    --   After loading once, further evaluations return the cached version.

    , _remote :: URL -> StateT Status IO Data.Text.Text
    -- ^ The remote resolver, fetches the content at the given URL.

    , _remoteBytes :: URL -> StateT Status IO Data.ByteString.ByteString
    -- ^ Like `_remote`, except for `Dhall.Syntax.Expr.Bytes`

    , _substitutions :: Dhall.Substitution.Substitutions Src Void

    , _normalizer :: Maybe (ReifiedNormalizer Void)

    , _startingContext :: Context (Expr Src Void)

    , _semanticCacheMode :: SemanticCacheMode

    , _cacheWarning :: CacheWarning
    -- ^ Records whether or not we already warned the user about issues with
    --   cache directory
    }

-- | Initial `Status`, parameterised over the HTTP 'Manager',
--   the origin headers and the remote resolver,
--   importing relative to the given root import.
emptyStatusWith
    :: IO Manager
    -> StateT Status IO OriginHeaders
    -> (URL -> StateT Status IO Data.Text.Text)
    -> (URL -> StateT Status IO Data.ByteString.ByteString)
    -> Import
    -> Status
emptyStatusWith _newManager _loadOriginHeaders _remote _remoteBytes rootImport = Status {..}
  where
    _stack = pure (Chained rootImport)

    _graph = []

    _cache = Map.empty

    _manager = Nothing

    _substitutions = Dhall.Substitution.empty

    _normalizer = Nothing

    _startingContext = Dhall.Context.empty

    _semanticCacheMode = UseSemanticCache

    _cacheWarning = CacheNotWarned

-- | Lens from a `Status` to its `_stack` field
stack :: Lens' Status (NonEmpty Chained)
stack = lens _stack (\s x -> s { _stack = x })

-- | Lens from a `Status` to its `_graph` field
graph :: Lens' Status [Depends]
graph = lens _graph (\s x -> s { _graph = x })

-- | Lens from a `Status` to its `_cache` field
cache :: Lens' Status (Map Chained ImportSemantics)
cache = lens _cache (\s x -> s { _cache = x })

-- | Lens from a `Status` to its `_remote` field
remote :: Lens' Status (URL -> StateT Status IO Data.Text.Text)
remote = lens _remote (\s x -> s { _remote = x })

-- | Lens from a `Status` to its `_remote` field
remoteBytes :: Lens' Status (URL -> StateT Status IO Data.ByteString.ByteString)
remoteBytes = lens _remoteBytes (\s x -> s { _remoteBytes = x })

-- | Lens from a `Status` to its `_substitutions` field
substitutions :: Lens' Status (Dhall.Substitution.Substitutions Src Void)
substitutions = lens _substitutions (\s x -> s { _substitutions = x })

-- | Lens from a `Status` to its `_normalizer` field
normalizer :: Lens' Status (Maybe (ReifiedNormalizer Void))
normalizer = lens _normalizer (\s x -> s {_normalizer = x})

-- | Lens from a `Status` to its `_startingContext` field
startingContext :: Lens' Status (Context (Expr Src Void))
startingContext = lens _startingContext (\s x -> s { _startingContext = x })

-- | Lens from a `Status` to its `_cacheWarning` field
cacheWarning :: Lens' Status CacheWarning
cacheWarning = lens _cacheWarning (\s x -> s { _cacheWarning = x })

{-| This exception indicates that there was an internal error in Dhall's
    import-related logic

    This exception indicates that an invalid `Dhall.Syntax.Type` was provided to
    the `Dhall.input` function
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

-- | Wrapper around `Network.HTTP.Client.HttpException`s with a prettier `Show`
-- instance
--
-- In order to keep the library API constant even when the @with-http@ Cabal
-- flag is disabled the pretty error message is pre-rendered and the real
-- 'Network.HTTP.Client.HttpException' is stored in a 'Dynamic'
data PrettyHttpException = PrettyHttpException String Dynamic
    deriving (Typeable)

instance Exception PrettyHttpException

instance Show PrettyHttpException where
  show (PrettyHttpException msg _) = msg
