{- | This module provides types and functions used in the substitution step
     which is done before type checking and normalization.
-}

module Dhall.Substitution where

import Data.Text       (Text)
import Dhall.Syntax    (Binding (..), Expr (..), FunctionBinding (..), Var (..))

import qualified Data.Foldable.WithIndex as Foldable.WithIndex
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
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

-- | Resolved substitution map plus the names that force a rebuild when a
-- binder is entered. Used so `shiftSubstitutions` can return the same map
-- pointer at binders that cannot capture a substitution.
data ResolvedSubstitutions s a = ResolvedSubstitutions
    { resolvedMap     :: Map.Map Var (Expr s a)
    , keyNames        :: Set.Set Text
    , valueFreeNames  :: Set.Set Text
    }

-- | @substitute expr s@ replaces all variables in @expr@ (or its subexpression) with their substitute.
--   For example, if the substitution map maps the variable @Foo@ to the text \"Foo\" all occurrences of @Foo@ with the text \"Foo\".
--
--   The substitutions will be done in the order they are inserted into the substitution map:
--
--   > {-# LANGUAGE OverloadedStrings #-}
--   >
--   > substitute (Dhall.Core.Var "Foo") (Dhall.Map.fromList [("Foo", Dhall.Core.Var "Bar"), ("Bar", Dhall.Core.Var "Baz")])
--
--   results in @Var \"Baz\"@ since \"Foo\"'s replacement (\"Bar\") is itself resolved against \"Bar\"'s substitution (\"Baz\") before being applied.
substitute :: Expr s a -> Substitutions s a -> Expr s a
substitute expr substitutions =
     substituteMany (resolveSubstitutions substitutions) expr

-- | Resolve insertion-order chaining, then compute identity-shift metadata
--   once. Import loading should cache this on 'Dhall.Import.Types.Status'
--   rather than calling 'substitute' (which re-resolves) per file.
--
--   Chaining itself uses the original always-shift walker. Calling 'fromMap'
--   on every prefix used to recompute 'freeVarNames' in O(N²) per
--   'substitute', which dominated as-code loads with hundreds of imports.
resolveSubstitutions :: Substitutions s a -> ResolvedSubstitutions s a
resolveSubstitutions substitutions =
     let step k v acc =
             Map.insert (V k 0) (substituteManyRaw acc v) acc

         resolvedMap' =
             Foldable.WithIndex.ifoldr
                 step
                 Map.empty
                 substitutions

     in  fromMap resolvedMap'

-- | Original substitution walker (always shift under binders). Used only
--   while chaining substitution values into each other.
substituteManyRaw :: Map.Map Var (Expr s a) -> Expr s a -> Expr s a
substituteManyRaw substitutions expression
     | Map.null substitutions = expression
substituteManyRaw substitutions (Var v) =
     Map.findWithDefault (Var v) v substitutions
substituteManyRaw substitutions (Lam cs (FunctionBinding src0 y src1 src2 type_) body) =
     let type_' = substituteManyRaw substitutions type_
         body' = substituteManyRaw (shiftSubstitutionsRaw y substitutions) body
     in Lam cs (FunctionBinding src0 y src1 src2 type_') body'
substituteManyRaw substitutions (Pi cs y domain codomain) =
     let domain' = substituteManyRaw substitutions domain
         codomain' = substituteManyRaw (shiftSubstitutionsRaw y substitutions) codomain
     in Pi cs y domain' codomain'
substituteManyRaw substitutions (Let (Binding src0 f src1 type_ src2 replacement) body) =
     let type_' = fmap (fmap (substituteManyRaw substitutions)) type_
         replacement' = substituteManyRaw substitutions replacement
         body' = substituteManyRaw (shiftSubstitutionsRaw f substitutions) body
     in Let (Binding src0 f src1 type_' src2 replacement') body'
substituteManyRaw substitutions expression =
     Lens.over Syntax.subExpressions (substituteManyRaw substitutions) expression

shiftSubstitutionsRaw
    :: Text -> Map.Map Var (Expr s a) -> Map.Map Var (Expr s a)
shiftSubstitutionsRaw name substitutions =
     let shiftKey (V k n) = if k == name then V k (n + 1) else V k n
         shiftValue = Syntax.shift 1 (V name 0)
         step k v = Map.insert (shiftKey k) (shiftValue v)
     in Map.foldrWithKey step Map.empty substitutions

fromMap :: Map.Map Var (Expr s a) -> ResolvedSubstitutions s a
fromMap resolvedMap' =
    ResolvedSubstitutions
        { resolvedMap = resolvedMap'
        , keyNames = Set.fromList [ k | V k _ <- Map.keys resolvedMap' ]
        , valueFreeNames = foldMap freeVarNames resolvedMap'
        }

substituteMany :: ResolvedSubstitutions s a -> Expr s a -> Expr s a
substituteMany substitutions expression
     | Map.null (resolvedMap substitutions) = expression
substituteMany substitutions (Var v) =
     Map.findWithDefault (Var v) v (resolvedMap substitutions)
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

-- | Shift substitution keys/values under a binder so we neither substitute the
-- newly bound variable nor capture it in substitution values.
--
-- If the binder name is not a substitution key and does not occur free in any
-- substitution value, the map is returned unchanged (same pointer). That is the
-- common case for Haskell-API substitutions (@Result@, schema names, …) under
-- ordinary @let@/@λ@ names.
shiftSubstitutions
    :: Text -> ResolvedSubstitutions s a -> ResolvedSubstitutions s a
shiftSubstitutions name substitutions
    | Set.notMember name (keyNames substitutions)
    , Set.notMember name (valueFreeNames substitutions) =
        substitutions
    | Set.notMember name (keyNames substitutions) =
        substitutions
            { resolvedMap =
                Map.map
                    (Syntax.shift 1 (V name 0))
                    (resolvedMap substitutions)
            }
    | otherwise =
        let shiftKey (V k n) = if k == name then V k (n + 1) else V k n
            shiftValue = Syntax.shift 1 (V name 0)
        in  substitutions
                { resolvedMap =
                    Map.mapKeys shiftKey
                        (Map.map shiftValue (resolvedMap substitutions))
                }

-- | Names of variables that occur free in an expression.
freeVarNames :: Expr s a -> Set.Set Text
freeVarNames = go Map.empty
  where
    go bound (Var (V k n)) =
        case Map.lookup k bound of
            Just depth | n < depth -> Set.empty
            _                      -> Set.singleton k
    go bound (Lam _ (FunctionBinding _ y _ _ type_) body) =
        go bound type_ <> go (Map.insertWith (+) y 1 bound) body
    go bound (Pi _ y domain codomain) =
        go bound domain <> go (Map.insertWith (+) y 1 bound) codomain
    go bound (Let (Binding _ f _ type_ _ replacement) body) =
           foldMap (go bound . snd) type_
        <> go bound replacement
        <> go (Map.insertWith (+) f 1 bound) body
    go bound expression =
        Lens.foldMapOf Syntax.subExpressions (go bound) expression
