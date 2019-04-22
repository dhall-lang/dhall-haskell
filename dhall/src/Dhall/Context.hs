{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Dhall.Context where

import Control.Monad.Reader
import Data.IORef
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import Data.Text (Text)
import Dhall.Binary
import Dhall.Core
import Dhall.Eval
import Network.HTTP.Client (Manager)
import System.FilePath

import qualified Data.Text


-- | An import which has been previously resolved during elaboration. The
--   contained 'Val' is the lazy value of the imported expression.
data ResolvedImport = ResolvedImport !Import Val

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
{-# inline quoteCxt #-}

define :: Text -> Val -> Val -> Cxt -> Cxt
define x t a (Cxt ts as) = Cxt (Extend ts x t) (TBind as x a)
{-# inline define #-}

bind :: Text -> Val -> Cxt -> Cxt
bind x a (Cxt ts as) = Cxt (Skip ts x) (TBind as x a)
{-# inline bind #-}


data ImportState = ImportState
  { _stack :: !(NonEmpty Import)
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

  , _cache :: !(IORef (Map Import (Core, Val, VType)))
    -- ^ Cache of imported expressions in order to avoid importing the same
    --   expression twice with different values

  , _manager :: !(IORef (Maybe Manager))
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

  , _standardVersion :: !StandardVersion
  }

type ElabM = ReaderT ImportState IO

emptyImportState :: FilePath -> IO ImportState
emptyImportState rootDirectory = do
  let
    prefix = if isRelative rootDirectory then Here else Absolute

    pathComponents =
      Data.Text.pack <$> Prelude.reverse (splitDirectories rootDirectory)

    dirAsFile = File (Directory pathComponents) "."

    -- Fake import to set the directory we're relative to.
    rootImport = Import
      { importHashed = ImportHashed
        { hash = Nothing
        , importType = Local prefix dirAsFile
        }
      , importMode = Code
      }

    _stack           = pure rootImport
    _manager         = Nothing
    _standardVersion = defaultStandardVersion

  _manager <- newIORef Nothing
  _cache   <- newIORef mempty
  pure (ImportState{..})
