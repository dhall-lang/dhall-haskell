{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Monad () where

import Dhall.Syntax.Instances.Applicative ()
import Dhall.Syntax.Types

import qualified Control.Monad

instance Monad (Expr s) where
    return = pure

    expression >>= k = Control.Monad.join $ pure k <*> expression
