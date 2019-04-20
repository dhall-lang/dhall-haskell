
module Dhall.Eval where

import {-# SOURCE #-} Dhall.Core

data Val
data Resolved

convEmpty      :: Expr X Resolved -> Expr X Resolved -> Bool
nfEmpty        :: Expr X Resolved -> Expr X X
alphaNormalize :: Expr s X -> Expr s X
