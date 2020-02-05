module Dhall.Substitution where

import Dhall.Core (subst)
import Dhall.Syntax (Expr, Var)

import qualified Dhall.Map

{- | Substitutions map variables to arbitrary Dhall expressions.
     Note that we use "Dhall.Map.Map" as an underlying structure. Hence we respect insertion order.
-}
type Substitutions s a = Dhall.Map.Map Var (Expr s a)

{- | An empty substitution map.
-}
empty :: Substitutions s a
empty = Dhall.Map.empty

-- | @substitute expr s@ replaces all variables in @expr@ (or its subexpression) with their substitute.
--   For example, if the substitution map maps the variable @Foo@ to the text \"Foo\" all occurrences of @Foo@ with the text \"Foo\".
--
--   The substitutions will be done in the order they are inserted into the substitution map:
--
--   > {-# LANGUAGE OverloadedStrings #-}
--   >
--   > substitute (Dhall.Core.Var "Foo") (Dhall.Map.fromList [("Foo", Dhall.Core.Var "Bar"), ("Bar", Dhall.Core.Var "Baz")])
--
--   results in @Dhall.Core.Var \"Baz\"@ since we first substitute \"Foo\" with \"Bar\" and then the resulting \"Bar\" with the final \"Baz\".
substitute :: Expr s a -> Substitutions s a -> Expr s a
substitute expr = foldl (\memo (k, v) -> subst k v memo) expr . Dhall.Map.toList
