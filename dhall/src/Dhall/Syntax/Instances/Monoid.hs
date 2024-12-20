{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Monoid () where

import Dhall.Syntax.Const (Const(..))
import Dhall.Syntax.Instances.Ord ()

instance Semigroup Const where
    (<>) = max

instance Monoid Const where
    mempty = Type
