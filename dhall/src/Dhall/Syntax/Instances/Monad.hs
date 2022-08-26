{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Monad () where

import Dhall.Syntax.Expr
import Dhall.Syntax.Instances.Applicative ()

import qualified Control.Monad

instance Monad (Expr s) where
    return = pure

    expression >>= k = Control.Monad.join $ pure k <*> expression
