{-# LANGUAGE StandaloneKindSignatures #-}

module Dhall.Syntax.Types where

import Data.Kind (Type)

data DhallDouble

data PreferAnnotation

type FieldSelection :: Type -> Type
data FieldSelection s

data WithComponent
