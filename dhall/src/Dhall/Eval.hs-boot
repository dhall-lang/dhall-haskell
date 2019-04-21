
module Dhall.Eval where

import {-# SOURCE #-} Dhall.Core

data Val
data Resolved

freeIn         :: Var -> Expr s a -> Bool
convEmpty      :: Expr X Resolved -> Expr X Resolved -> Bool
nfEmpty        :: Expr X Resolved -> Expr X X
alphaNormalize :: Expr s X -> Expr s X

type Core = Expr X Resolved
type Nf   = Expr X X
