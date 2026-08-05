{- | This module provides types and functions used in the substitution step
     which is done before type checking and normalization.
-}

module Dhall.Substitution where

import Data.Text       (Text)
import Dhall.Syntax    (Binding (..), Expr (..), FunctionBinding (..), Var (..))

import qualified Data.Foldable.WithIndex as Foldable.WithIndex
import qualified Data.Map.Strict         as Map
import qualified Dhall.Map
import qualified Dhall.Syntax            as Syntax
import qualified Lens.Micro              as Lens

{- | Substitutions map variables to arbitrary Dhall expressions.
     Note that we use "Dhall.Map.Map" as an underlying structure. Hence we respect insertion order.
-}
type Substitutions s a = Dhall.Map.Map Text (Expr s a)

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
--   results in @Dhall.Core.Var \"Baz\"@ since \"Foo\"'s replacement (\"Bar\") is itself resolved against \"Bar\"'s substitution (\"Baz\") before being applied.
substitute :: Expr s a -> Substitutions s a -> Expr s a
substitute expr substitutions =
     substituteMany (resolveSubstitutions substitutions) expr

resolveSubstitutions :: Substitutions s a -> Map.Map Var (Expr s a)
resolveSubstitutions substitutions =
     let step k v resolved = Map.insert (V k 0) (substituteMany resolved v) resolved
     in Foldable.WithIndex.ifoldr step Map.empty substitutions

substituteMany :: Map.Map Var (Expr s a) -> Expr s a -> Expr s a
substituteMany substitutions expression
     | Map.null substitutions = expression
substituteMany substitutions (Var v) =
     Map.findWithDefault (Var v) v substitutions
substituteMany substitutions (Lam cs (FunctionBinding src0 y src1 src2 type_) body) =
     let type_' = substituteMany substitutions type_
         body' = substituteMany (shiftSubstitutions y substitutions) body
     in Lam cs (FunctionBinding src0 y src1 src2 type_') body'
substituteMany substitutions (Pi cs y domain codomain) =
     let domain' = substituteMany substitutions domain
         codomain' = substituteMany (shiftSubstitutions y substitutions) codomain
     in Pi cs y domain' codomain'
substituteMany substitutions (Let (Binding src0 f src1 type_ src2 replacement) body) =
     let type_' = fmap (fmap (substituteMany substitutions)) type_
         replacement' = substituteMany substitutions replacement
         body' = substituteMany (shiftSubstitutions f substitutions) body
     in Let (Binding src0 f src1 type_' src2 replacement') body'
substituteMany substitutions expression =
     Lens.over Syntax.subExpressions (substituteMany substitutions) expression

shiftSubstitutions :: Text -> Map.Map Var (Expr s a) -> Map.Map Var (Expr s a)
shiftSubstitutions name substitutions =
     let shiftKey (V k n) = if k == name then V k (n + 1) else V k n
         shiftValue = Syntax.shift 1 (V name 0)
         step k v = Map.insert (shiftKey k) (shiftValue v)
     in Map.foldrWithKey step Map.empty substitutions
