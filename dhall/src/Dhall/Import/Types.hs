{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}

module Dhall.Import.Types where

import Control.Exception (Exception)
import Data.Dynamic
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Tree (Tree)
import Dhall.Binary (StandardVersion(..))
import Dhall.Context (Context)
import Dhall.Core
  ( Chunks(..)
  , Directory (..)
  , Expr(..)
  , File (..)
  , FilePrefix (..)
  , ImportMode (..)
  , ReifiedNormalizer(..)
  , URL(..)
  )
import Dhall.Parser (Src)
import Dhall.TypeCheck (X)
import Lens.Family (LensLike')
import System.FilePath (isRelative, splitDirectories, takeFileName, takeDirectory)

import qualified Crypto.Hash
import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Dhall.Map
import qualified Data.Map
import qualified Data.Text

-- HTTP headers
type Headers = [(Text, Text)]

-- non-cyclic, binary headers, canonical
data ResolvedImportType
    = ResolvedLocal FilePrefix File
      -- ^ Like a normal file import, but not resolved relative to the import stack
    | ResolvedRemote URL (Maybe Headers)
      -- ^ URL (sans headers) plus normalised headers
    | ResolvedEnv Text  -- ^ same as plain Env
    | ResolvedMissing  -- ^ same as plain Missing
    deriving (Eq, Ord)

instance Pretty ResolvedImportType where
    pretty (ResolvedLocal prefix file) =
        pretty prefix <> pretty file

    pretty (ResolvedRemote url Nothing) = pretty url

    pretty (ResolvedRemote url (Just headers)) =
        pretty url <> " using " <> pretty headersExpr
      where
        headersExpr = ListLit Nothing (foldMap (pure . headerExpr) headers)
        headerExpr :: (Text, Text) -> Expr Src X
        headerExpr (key, value) =
            let keyExpr = TextLit (Chunks [] key)
                valueExpr = TextLit (Chunks [] value)
            in RecordLit (Dhall.Map.fromList [("mapKey", keyExpr), ("mapValue", valueExpr)])

    pretty (ResolvedEnv env) = "env:" <> pretty env

    pretty ResolvedMissing = "missing"


type SemanticHash = Crypto.Hash.Digest Crypto.Hash.SHA256
type SemisemanticHash = Crypto.Hash.Digest Crypto.Hash.SHA256

data ResolvedImport = ResolvedImport (Maybe SemanticHash) ResolvedImportType ImportMode
    deriving (Eq, Ord)
instance Pretty ResolvedImport where
    pretty (ResolvedImport maybeHash resolvedImportType mode) =
        pretty resolvedImportType <> prettyHash <> prettyMode
      where
        prettyHash = case maybeHash of
            Just hash -> " sha256:" <> pretty (show hash)
            Nothing -> ""
        prettyMode = case mode of
            RawText  -> " as Text"
            Location -> " as Location"
            Code     -> ""


data SemanticImport = SemanticImport
    { normalisedImport :: Expr Src X
    -- ^ typechecked and alpha-beta-normal
    , semanticHash :: SemanticHash
    , graph :: Tree ResolvedImport  -- `Real` resolved import is root node
    }

data LoadedExpr = LoadedExpr
    { loadedExpr :: Expr Src X  -- not typechecked or normalised!
    , imports :: [SemanticImport]
    }

type ImportStack = NonEmpty ResolvedImport

-- | State threaded throughout the import process
data Status = Status
    { _cache :: Data.Map.Map ResolvedImport SemanticImport
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
emptyStatus :: Status
emptyStatus = Status {..}
  where
    _cache = Data.Map.empty

    _manager = Nothing

    _standardVersion = Dhall.Binary.defaultStandardVersion

    _normalizer = Nothing

    _startingContext = Dhall.Context.empty


-- | Given a path to dhall file (can be a dummy path like "~/.") construct the
-- corresponding ResolvedImport to be used as the root import on the import stack.
rootImport :: FilePath -> ResolvedImport
rootImport filePath = ResolvedImport Nothing resolvedImportType Code
  where
    file = Data.Text.pack (takeFileName filePath)
    directory = takeDirectory filePath
    prefix = if isRelative filePath
        then Here
        else Absolute

    directoryComponents =
        fmap Data.Text.pack (reverse (splitDirectories directory))

    resolvedImportType = ResolvedLocal prefix (File (Directory directoryComponents) file)

cache :: Functor f => LensLike' f Status (Data.Map.Map ResolvedImport SemanticImport)
cache k s = fmap (\x -> s { _cache = x }) (k (_cache s))

manager :: Functor f => LensLike' f Status (Maybe Dynamic)
manager k s = fmap (\x -> s { _manager = x }) (k (_manager s))

standardVersion :: Functor f => LensLike' f Status StandardVersion
standardVersion k s =
    fmap (\x -> s { _standardVersion = x }) (k (_standardVersion s))

normalizer :: Functor f => LensLike' f Status (Maybe (ReifiedNormalizer X))
normalizer k s = fmap (\x -> s {_normalizer = x}) (k (_normalizer s))

startingContext :: Functor f => LensLike' f Status (Context (Expr Src X))
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
