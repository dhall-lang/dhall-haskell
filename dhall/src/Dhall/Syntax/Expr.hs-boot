{-# LANGUAGE StandaloneKindSignatures #-}

module Dhall.Syntax.Expr where

import Data.Kind (Type)

type Expr :: Type -> Type -> Type
data Expr s a
