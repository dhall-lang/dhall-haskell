{-| This module contains logic for pretty-printing expressions, including
    support for syntax highlighting
-}
module Dhall.Pretty
    ( -- * Pretty
      Ann(..)
    , annToAnsiStyle
    , prettyExpr
    , layoutOpts
    ) where

import Dhall.Pretty.Internal
import qualified Data.Text.Prettyprint.Doc as Pretty

-- | Default layout options
layoutOpts :: Pretty.LayoutOptions
layoutOpts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

