{-| Both the GHC and GHCJS implementations of "Dhall.Import.Manager" export a
    `Manager` type suitable for use within the "Dhall.Import" module

    For the GHC implementation the `Manager` type is a real `Manager` from the
    @http-client@ package.  For the GHCJS implementation the `Manager` type is
    a synonym for @`Data.Void.Void`@ since GHCJS does not use a `Manager` for
    HTTP requests.
-}
module Dhall.Import.Manager
    ( -- * Manager
      Manager
    ) where

import Data.Void (Void)

-- | GHCJS does not use a `Manager`
type Manager = Void
