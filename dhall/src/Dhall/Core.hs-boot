{-# LANGUAGE RankNTypes #-}

module Dhall.Core where

data Const

data Var

data Expr s a

data Import

newtype X = X { absurd :: forall a . a }
