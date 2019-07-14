module Dhall.Binary where

import Codec.CBOR.Term (Term)

import {-# SOURCE #-} Dhall.Core

class ToTerm a where
    encode :: a -> Term

instance ToTerm Nf

class FromTerm a where
    decode :: Term -> Maybe a
