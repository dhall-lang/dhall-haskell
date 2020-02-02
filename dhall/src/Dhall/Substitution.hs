module Dhall.Substitution where

import Dhall.Core (subst)
import Dhall.Syntax (Expr, Var)

import qualified Data.Map

{- | Substitutions map variables to arbitrary Dhall expressions.
-}
type Substitutions s a = Data.Map.Map Var (Expr s a)

{- | An empty substitution map.
-}
empty :: Substitutions s a
empty = Data.Map.empty

{- | `substitute expr s` replaces all variables in `expr` (or its subexpression) with their substitute.
     For example, if the substitution map maps the Variable `Foo` to the text \"Foo\" all occurrences of `Foo` with the text \"Foo\".
-}
substitute :: Expr s a -> Substitutions s a -> Expr s a
substitute = Data.Map.foldrWithKey subst
