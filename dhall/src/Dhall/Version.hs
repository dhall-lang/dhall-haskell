{-| Utilities for getting the current version of the Haskell implementation of
    Dhall
-}
module Dhall.Version ( dhallVersion
                     , dhallVersionString
                     ) where

import qualified Data.Version as V
import qualified Paths_dhall  as P

-- | The current `Version` of the Haskell implementation
dhallVersion :: V.Version
dhallVersion = P.version

-- | The current version `String` for the Haskell implementation
dhallVersionString :: String
dhallVersionString = V.showVersion dhallVersion
