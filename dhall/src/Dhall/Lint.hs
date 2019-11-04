{-# LANGUAGE RecordWildCards #-}

-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    , removeUnusedBindings
    ) where

import Control.Applicative ((<|>))
import Dhall.Syntax (Binding(..), Expr(..), Import, Var(..), subExpressions)

import qualified Dhall.Core
import qualified Dhall.Optics
import qualified Lens.Family

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeUnusedBindings'.
    * fixes @let a = x ≡ y@ to be @let a = assert : x ≡ y@
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeLetInLet'
-}
lint :: Expr s Import -> Expr t Import
lint =
      Dhall.Optics.rewriteOf
        subExpressions
        (\e -> fixAsserts e <|> removeUnusedBindings e)
    . removeLetInLet

-- | Remove unused `Let` bindings.
removeUnusedBindings :: Eq a => Expr s a -> Maybe (Expr s a)
-- Don't remove assertions!
removeUnusedBindings (Let (Binding _ _ _ _ _ e) _)
    | isOrContainsAssert e = Nothing
removeUnusedBindings (Let (Binding _ a _ _ _ _) d)
    | not (V a 0 `Dhall.Core.freeIn` d) =
        Just (Dhall.Core.shift (-1) (V a 0) d)
removeUnusedBindings _ = Nothing

-- Fix `Let` bindings  that the user probably meant to be `assert`s
fixAsserts :: Expr s a -> Maybe (Expr s a)
fixAsserts (Let (Binding { value = Equivalent x y, ..}) body) =
    Just (Let (Binding { value = Assert (Equivalent x y), .. }) body)
fixAsserts _ =
    Nothing

isOrContainsAssert :: Expr s a -> Bool
isOrContainsAssert (Assert _) = True
isOrContainsAssert e = Lens.Family.anyOf subExpressions isOrContainsAssert e

-- The difference between
--
-- > let x = 1 let y = 2 in x + y
--
-- and
--
-- > let x = 1 in let y = 2 in x + y
--
-- is that in the second expression, the inner 'Let' is wrapped by a 'Note'.
--
-- Denoting removes that distinction.
removeLetInLet :: Expr s a -> Expr t a
removeLetInLet = Dhall.Core.denote
