{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Monoid () where

import Dhall.Syntax.Instances.Semigroup ()
import Dhall.Syntax.Types

instance Monoid (Chunks s a) where
    mempty = Chunks [] mempty
