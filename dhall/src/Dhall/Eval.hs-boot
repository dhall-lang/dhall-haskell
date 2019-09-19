
module Dhall.Eval where

import {-# SOURCE #-} Dhall.Core

judgmentallyEqual :: Eq a => Expr s a -> Expr t a -> Bool
normalize         :: Eq a => Expr s a -> Expr t a
alphaNormalize    :: Expr s a -> Expr s a
