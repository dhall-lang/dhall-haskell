{-| This module contains logic for pretty-printing expressions, including
    support for syntax highlighting
-}
module Dhall.Pretty
    ( -- * Pretty
      Ann(..)
    , annToAnsiStyle
    , prettyExpr

    , CharacterSet(..)
    , prettyCharacterSet

    , layout
    , layoutOpts
    ) where

import Dhall.Pretty.Internal
import qualified Data.Text.Prettyprint.Doc as Pretty

-- | Layout using 'layoutOpts'
--
-- Tries hard to fit the document into 80 columns.
--
-- Removes trailing whitespace.
layout :: Pretty.Doc ann -> Pretty.SimpleDocStream ann
layout = Pretty.removeTrailingWhitespace . Pretty.layoutSmart layoutOpts

-- | Default layout options
layoutOpts :: Pretty.LayoutOptions
layoutOpts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
