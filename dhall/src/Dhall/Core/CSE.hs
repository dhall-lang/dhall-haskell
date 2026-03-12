{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the implementation of the @dhall cse@ subcommand
--   (Common Subexpression Elimination)

module Dhall.Core.CSE
    ( -- * CSE
      cse
    ) where

import Data.Map.Strict (Map)
import Data.HashSet    (HashSet)
import Data.Text       (Text)
import Data.Void       (Void)
import Dhall.Syntax
    ( Binding (..)
    , Chunks (..)
    , Expr (..)
    , FunctionBinding (..)
    , Var (..)
    , makeBinding
    , subExpressions
    , reservedIdentifiers
    )

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Ord        as Ord
import qualified Data.HashSet    as HashSet
import qualified Data.Text       as Text
import qualified Lens.Micro      as Lens

{-| Eliminate common subexpressions in a Dhall expression.

    This function finds subexpressions that appear more than once in the
    expression, extracts them into @let@ bindings, and replaces all
    occurrences with references to those bindings.

    For example, given an expression like:

    > [ < A | B >.A, < A | B >.B ]

    This function will return:

    > let _1 = < A | B > in [ _1.A, _1.B ]

    Only subexpressions that are not trivial (not a variable, constant,
    literal, or built-in) and that contain no free variables are eligible
    for extraction.
-}
cse :: forall a. Ord a => Expr Void a -> Expr Void a
cse expr = go freshNames candidates expr
  where
    -- Count occurrences of each subexpression using cosmosOf to traverse all levels
    counts :: Map (Expr Void a) Int
    counts = Map.fromListWith (+)
        [ (sub, 1)
        | sub <- Lens.foldMapOf (Lens.cosmosOf subExpressions) (:[]) expr
        ]

    -- Collect all names used in the expression (for fresh name generation)
    usedNames :: HashSet Text
    usedNames = Lens.foldMapOf (Lens.cosmosOf subExpressions) exprNames expr

    -- Generate an infinite list of fresh names not conflicting with used names
    -- or reserved Dhall identifiers
    freshNames :: [Text]
    freshNames =
        filter (not . flip HashSet.member reservedNames)
            [ "_" <> Text.pack (show i) | i <- [(1 :: Int) ..] ]
      where
        reservedNames = usedNames `HashSet.union` reservedIdentifiers

    -- Find candidates: subexpressions that appear >= 2 times, are non-trivial,
    -- and contain no free variables.
    -- Sort by size descending so that the largest common subexpressions are
    -- extracted first (this maximises the size reduction).
    candidates :: [Expr Void a]
    candidates =
        List.sortBy (Ord.comparing (Ord.Down . exprSize))
            [ e
            | (e, count) <- Map.toList counts
            , count >= 2
            , shouldExtract e
            , hasNoVars e
            ]

    -- Process candidates one by one, extracting those that still appear
    -- >= 2 times in the current (possibly already-transformed) expression.
    go :: [Text] -> [Expr Void a] -> Expr Void a -> Expr Void a
    go _              []                   currentExpr = currentExpr
    go []             _                    currentExpr = currentExpr
    go (name : names) (candidate : rest)   currentExpr =
        if countOccurrences candidate currentExpr >= 2
        then
            let body    = replaceAll candidate (Var (V name 0)) currentExpr
                newExpr = Let (makeBinding name candidate) body
            in  go names rest newExpr
        else
            go (name : names) rest currentExpr


-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Count how many times @target@ appears as a subexpression of @expr@.
countOccurrences :: Eq a => Expr Void a -> Expr Void a -> Int
countOccurrences target expr =
    length
        [ ()
        | sub <- Lens.foldMapOf (Lens.cosmosOf subExpressions) (:[]) expr
        , sub == target
        ]

-- | Replace every occurrence of @old@ with @new@ in @expr@, top-down.
--   If @expr == old@, return @new@ without recursing into @new@.
replaceAll :: Eq a => Expr Void a -> Expr Void a -> Expr Void a -> Expr Void a
replaceAll old new expr
    | expr == old = new
    | otherwise   = Lens.over subExpressions (replaceAll old new) expr

-- | Decide whether an expression is worth extracting.
--
--   We do NOT extract:
--   * Variables, type constants, and built-in names / functions
--     (they are already compact single tokens)
--   * Simple literals (Bool, Natural, Integer, Double, plain Text, Bytes,
--     Date/Time literals)
--   * Embedded import values (they are already references)
--
--   Everything else (union types, record types, applications, lambdas, etc.)
--   is eligible for extraction.
shouldExtract :: Expr s a -> Bool
shouldExtract = \case
    Var _                 -> False
    Const _               -> False
    Bool                  -> False
    BoolLit _             -> False
    Bytes                 -> False
    BytesLit _            -> False
    Natural               -> False
    NaturalLit _          -> False
    NaturalFold           -> False
    NaturalBuild          -> False
    NaturalIsZero         -> False
    NaturalEven           -> False
    NaturalOdd            -> False
    NaturalToInteger      -> False
    NaturalShow           -> False
    NaturalSubtract       -> False
    Integer               -> False
    IntegerLit _          -> False
    IntegerClamp          -> False
    IntegerNegate         -> False
    IntegerShow           -> False
    IntegerToDouble       -> False
    Double                -> False
    DoubleLit _           -> False
    DoubleShow            -> False
    Text                  -> False
    TextLit (Chunks [] _) -> False  -- plain string, no interpolation
    TextReplace           -> False
    TextShow              -> False
    Date                  -> False
    DateLiteral _         -> False
    DateShow              -> False
    Time                  -> False
    TimeLiteral _ _       -> False
    TimeShow              -> False
    TimeZone              -> False
    TimeZoneLiteral _     -> False
    TimeZoneShow          -> False
    List                  -> False
    ListBuild             -> False
    ListFold              -> False
    ListLength            -> False
    ListHead              -> False
    ListLast              -> False
    ListIndexed           -> False
    ListReverse           -> False
    Optional              -> False
    None                  -> False
    Embed _               -> False
    _                     -> True   -- all other constructors are non-trivial

-- | Return 'True' if the expression contains no 'Var' constructors.
--
--   This is a conservative check for "has no free variables": any expression
--   that mentions a variable name might depend on a surrounding binder, so we
--   skip those.  Expressions that pass this check (e.g. union types, record
--   types, fully-applied built-ins) are safe to lift to a @let@ binding at
--   the top of the expression.
hasNoVars :: Expr s a -> Bool
hasNoVars = not . Lens.anyOf (Lens.cosmosOf subExpressions) isVar
  where
    isVar (Var _) = True
    isVar _       = False

-- | Compute the total number of nodes in the expression tree.
--   Used to sort candidates so that the largest are extracted first.
exprSize :: Expr Void a -> Int
exprSize = length . Lens.foldMapOf (Lens.cosmosOf subExpressions) (:[])

-- | Collect all variable and binder names that appear in a single expression
--   node (not recursively - this is called via 'Lens.cosmosOf' which handles
--   the recursive traversal).
--   Used to generate fresh names for CSE bindings.
exprNames :: Expr s a -> HashSet Text
exprNames = \case
    Var (V name _) ->
        HashSet.singleton name
    Lam _ FunctionBinding{functionBindingVariable = x} _ ->
        HashSet.singleton x
    Pi  _ x _ _ ->
        HashSet.singleton x
    Let (Binding _ x _ _ _ _) _ ->
        HashSet.singleton x
    _ ->
        HashSet.empty
