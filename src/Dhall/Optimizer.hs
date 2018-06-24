{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dhall.Optimizer (
  Optimizer,
  optimizer,
  ) where

import Dhall.Core
import Dhall.TH

type Optimizer s a = Expr s a -> Expr s a

optNaturalMonus :: (Eq s, Eq a) => Optimizer s a
optNaturalMonus e =
  case e of
    App (App f (NaturalLit a)) (NaturalLit b) | f `cmp` ast -> NaturalLit (if a > b then a - b else 0)
    _ -> e
  where
    ast = $(staticDhallExpression "./src/Dhall/Optimizer/NaturalMonus")
    cmp el er = alphaNormalize el == alphaNormalize er

optimizations :: (Eq s, Eq a) => [Expr s a -> Expr s a]
optimizations = optNaturalMonus : []

optimizer :: (Eq s, Eq a) => Optimizer s a
optimizer e = foldr id e optimizations
