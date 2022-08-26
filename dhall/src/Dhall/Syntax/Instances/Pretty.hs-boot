{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Pretty where

import Dhall.Syntax.Expr (Expr)
import Prettyprinter     (Pretty)

instance Pretty a => Pretty (Expr s a)
