{-# OPTIONS_GHC -Wall #-}

module Dhall.Import.Types where

import Control.Exception (Exception)
import Data.Dynamic
import Data.Map (Map)
import Data.Semigroup ((<>))
import Lens.Family (LensLike')

import qualified Data.Map as Map

import Dhall.Core (Import, Expr)
import Dhall.Parser (Src)
import Dhall.TypeCheck (X)


-- | State threaded throughout the import process
data Status = Status
    { _stack   :: [Import]
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point
    , _cache   :: Map Import (Expr Src X)
    -- ^ Cache of imported expressions in order to avoid importing the same
    --   expression twice with different values
    , _manager :: Maybe Dynamic
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once
    }

-- | Default starting `Status`
emptyStatus :: Status
emptyStatus = Status [] Map.empty Nothing

stack :: Functor f => LensLike' f Status [Import]
stack k s = fmap (\x -> s { _stack = x }) (k (_stack s))

cache :: Functor f => LensLike' f Status (Map Import (Expr Src X))
cache k s = fmap (\x -> s { _cache = x }) (k (_cache s))

manager :: Functor f => LensLike' f Status (Maybe Dynamic)
manager k s = fmap (\x -> s { _manager = x }) (k (_manager s))



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
