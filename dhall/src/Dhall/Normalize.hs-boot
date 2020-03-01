module Dhall.Normalize where

import Dhall.Syntax

desugarWith :: Expr s a -> Expr s a
