module Dhall.Version ( dhallVersion
                     , dhallVersionString
                     ) where

import qualified Data.Version as V
import qualified Paths_dhall  as P

dhallVersion :: V.Version
dhallVersion = P.version

dhallVersionString :: String
dhallVersionString = V.showVersion dhallVersion
