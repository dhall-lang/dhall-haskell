{-| Both the GHC and GHCJS implementations of 'Dhall.Import.Manager.Manager'
    export a `Dhall.Import.Manager.Manager` type suitable for use within the
    "Dhall.Import" module

    For the GHC implementation the `Dhall.Import.Manager` type is a real
    `Network.HTTP.Client.Manager` from the @http-client@ package.  For the
    GHCJS implementation the `Dhall.Import.Manager.Manager` type is
    a synonym for @`Data.Void.Void`@ since GHCJS does not use a
    `Network.HTTP.Client.Manager` for HTTP requests.
-}
module Dhall.Import.Manager
    ( -- * Manager
      Manager
    ) where

import Network.HTTP.Client (Manager)
