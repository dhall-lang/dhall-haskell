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
import qualified Data.Map      as Map
import qualified Data.Text

-- HTTP headers
type Headers = [(Text, Text)]

-- non-cyclic, binary headers, canonical
data ResolvedImportType =
    ResolvedLocal FilePrefix File
    -- ^ Like a normal file import, but not resolved relative to the import stack
    ResolvedRemote URL (Maybe Headers)
    -- ^ URL (sans headers) plus normalised headers
    ResolvedEnv Text  -- ^ same as plain Env
    ResolvedMissing  -- ^ same as plain Missing

instance Pretty ResolvedImportType where
    pretty (ResolvedLocal prefix file) =
        Pretty.pretty prefix <> Pretty.pretty file

    pretty (ResolvedRemote url Nothing) = Pretty.pretty url

    pretty (ResolvedRemote url (Just headers)) =
        Pretty.pretty url <> " using " <> Pretty.pretty headersExpr
      where
        headersExpr = ListLit Nothing (foldMap headerExpr headers)
        headerExpr (key, value) =
            let keyExpr = TextLit (Chunks [] key)
                valueExpr = TextLit (Chunks [] value)
            in RecordLit (Map.fromList [("mapKey", keyExpr), ("mapValue", valueExpr)]

    pretty (ResolvedEnv env) = "env:" <> Pretty.pretty env

    pretty Missing = "missing"


newtype SemanticHash = SemanticHash { fromSemanticHash :: Crypto.Hash.Digest SHA256 }
newtype SemisemanticHash = SemisemanticHash { fromSemisemanticHash :: Crypto.Hash.Digest SHA256 }

data ResolvedImport = ResolvedImport (Maybe SemanticHash) ResolvedImportType ImportMode

instance Pretty ResolvedImport where
    pretty (Import {..}) = Pretty.pretty importHashed <> Pretty.pretty suffix
      where
        suffix :: Text
        suffix = case importMode of
            RawText  -> " as Text"
            Location -> " as Location"
            Code     -> ""


data SemanticImport = SemanticImport
    { normalisedImport :: Expr Src X
    -- ^ typechecked and normalised (not necessarily alpha-normal)
    , semanticHash :: SemanticHash
    , graph :: Tree ResolvedImport  -- `Real` resolved import is root node
    }

data LoadedExpr = LoadedExpr
    { loadedExpr :: Expr Src X  -- not typechecked or normalised!
    , imports :: [SemanticImport]
    }

data ImportStack = NonEmpty ResolvedImport

-- | State threaded throughout the import process
data Status m = Status
    { _cache :: Map Import SemanticImport
    -- ^ Cache of imported expressions with their node id in order to avoid
    --   importing the same expression twice with different values

    , _manager :: Maybe Dynamic
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

    , _standardVersion :: StandardVersion

    , _normalizer :: Maybe (ReifiedNormalizer X)
    -- ^ TODO!

    , _startingContext :: Context (Expr Src X)
    -- ^ typechecking context; allows us to load _open_ expressions
    }

-- | Default starting `Status` that is polymorphic in the base `Monad`
emptyStatus :: Status m
emptyStatus = Status {..}
  where
    _cache = Map.empty

    _manager = Nothing

    _standardVersion = Dhall.Binary.defaultStandardVersion

    _normalizer = Nothing

    _startingContext = Dhall.Context.empty


-- TODO: allow file names as well (not just directory)
rootImport :: FilePath -> ResolvedImport
rootImport filePath = ResolvedImport Nothing importType Code
  where
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

cache :: Functor f => LensLike' f (Status m) (Map ResolvedImport SemanticImport)
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
