{-# LANGUAGE StandaloneKindSignatures #-}

module Dhall.Syntax.Chunks where

import Data.Kind (Type)

type Chunks :: Type -> Type -> Type
data Chunks s a
