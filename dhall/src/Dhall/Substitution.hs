module Dhall.Substitution where

import Dhall.Core (subst)
import Dhall.Syntax (Expr, Var)

import qualified Data.Map

type Substitutions s a = Data.Map.Map Var (Expr s a)

empty :: Substitutions s a
empty = Data.Map.empty

substitute :: Expr s a -> Substitutions s a -> Expr s a
substitute = Data.Map.foldrWithKey subst
