{-| This module contains logic for pretty-printing expressions, including
    support for syntax highlighting
-}
module Dhall.Pretty
    ( -- * Pretty
      Ann(..)
    , annToAnsiStyle
    , prettyExpr

    , CharacterSet(..)
    , detectCharacterSet
    , prettyCharacterSet

    , Dhall.Pretty.Internal.layout
    , Dhall.Pretty.Internal.layoutOpts

    , escapeEnvironmentVariable
    , escapeLabel

    , temporalToText
    ) where

import Dhall.Pretty.Internal
