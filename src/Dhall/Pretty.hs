{-| This module contains logic for pretty-printing expressions, including
    support for syntax highlighting
-}
module Dhall.Pretty ( Ann(..), annToAnsiStyle, prettyExpr ) where

import Dhall.Pretty.Internal
