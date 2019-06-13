-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Dhall.Core (Binding(..), Expr(..), Import, Var(..), subExpressions)

import qualified Dhall.Core
import qualified Dhall.Optics

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeLetInLet'.
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeUnusedBindings'.
-}
lint :: Expr s Import -> Expr t Import
lint = postproc . linting . preproc
  where
    -- pre-processing: remove Note constructors and unfold Let blocks
    preproc = Dhall.Optics.rewriteOf subExpressions unfoldNestedLets . Dhall.Core.denote
    -- main linting step: remove unused let bindings and update optional syntax
    linting = Dhall.Optics.rewriteOf subExpressions removeUnusedBindings
    -- post-processing: fold nested Lets into Let blocks
    postproc = Dhall.Optics.rewriteOf subExpressions removeLetInLet

-- Merge nested Let blocks. Only finds immediately nested blocks -- use
-- Dhall.Core.denote to make sure there aren't any Note constructors in the way!
removeLetInLet :: Eq a => Expr s a -> Maybe (Expr s a)
removeLetInLet (Let a (Let b c)) = Just (Let (a <> b) c)
removeLetInLet _ = Nothing

-- Remove unused Let bindings. Only considers Let blocks binding a single
-- variable -- unfold Let blocks first to make sure we don't miss any rewrite
-- opportunities!
removeUnusedBindings :: Eq a => Expr s a -> Maybe (Expr s a)
removeUnusedBindings (Let (Binding a _ _ :| []) d)
    | not (V a 0 `Dhall.Core.freeIn` d) =
        Just (Dhall.Core.shift (-1) (V a 0) d)
removeUnusedBindings _ = Nothing

-- Unfold Let blocks into nested Let bindings binding a single variable each.
-- Pre-processing step before applying the removeUnusedBindings rule.
unfoldNestedLets :: Expr s a -> Maybe (Expr s a)
unfoldNestedLets (Let (b :| (l : ls)) d) = Just (Let (b :| []) (Let (l :| ls) d))
unfoldNestedLets _ = Nothing
