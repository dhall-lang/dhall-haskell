{-| This module contains logic for pretty-printing expressions, including
    support for syntax highlighting
-}
module Dhall.Pretty
    ( -- * Pretty
      Ann(..)
    , annToAnsiStyle
    , prettyExpr

    , CharacterSet(..)
    , defaultCharacterSet
    , detectCharacterSet
    , prettyCharacterSet

    , ChooseCharacterSet(..)
    , chooseCharsetOrUseDefault

    , Dhall.Pretty.Internal.layout
    , Dhall.Pretty.Internal.layoutOpts

    , escapeEnvironmentVariable
    , UnescapedLabel(..)
    , escapeLabel

    , temporalToText
    ) where

import Dhall.Pretty.Internal
