
module Dhall.Eval where

import {-# SOURCE #-} Dhall.Core

convEmpty      :: Eq a => Expr s a -> Expr t a -> Bool
nfEmpty        :: Eq a => Expr s a -> Expr t a
alphaNormalize :: Expr s a -> Expr s a
