{-| Both the GHC and JavaScript implementations of `Dhall.Import.Manager.Manager`
    export a `Dhall.Import.Manager.Manager` type suitable for use within the
    "Dhall.Import" module

    For the GHC implementation the `Dhall.Import.Manager.Manager` type is a real
    `Network.HTTP.Client.Manager` from the @http-client@ package.  For the
    JavaScript backend the `Dhall.Import.Manager.Manager` type is @()@ since
    that backend does not use a `Network.HTTP.Client.Manager` for HTTP requests.
-}
module Dhall.Import.Manager
    ( -- * Manager
      Manager
    , defaultNewManager
    ) where

{-| The JavaScript implementation does not require a `Network.HTTP.Client.Manager`

    The purpose of this synonym is so that "Dhall.Import.Types" can import a
    `Dhall.Import.Manager.Manager` type from "Dhall.Import.HTTP" that does the
    correct thing for both the GHC and JavaScript implementations
-}
type Manager = ()

defaultNewManager :: IO Manager
defaultNewManager = pure ()
