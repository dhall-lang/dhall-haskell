module Dhall.Binary where

import Codec.CBOR.Term (Term)

import {-# SOURCE #-} Dhall.Core

class ToTerm a where
    encode :: a -> Term

instance ToTerm a => ToTerm (Expr s a)
