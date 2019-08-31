-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    ) where

import Dhall.Core (Expr(..), Import, Var(..), subExpressions)

import qualified Dhall.Core
import qualified Dhall.Optics

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeUnusedBindings'.
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeLetInLet'
-}
lint :: Expr s Import -> Expr t Import
lint = Dhall.Optics.rewriteOf subExpressions removeUnusedBindings . removeLetInLet

-- Remove unused Let bindings. Only considers Let blocks binding a single
-- variable -- unfold Let blocks first to make sure we don't miss any rewrite
-- opportunities!
removeUnusedBindings :: Eq a => Expr s a -> Maybe (Expr s a)
-- Don't remove assertions!
removeUnusedBindings (Let _ _ (Assert _) _) = Nothing
removeUnusedBindings (Let a _ _ d)
    | not (V a 0 `Dhall.Core.freeIn` d) =
        Just (Dhall.Core.shift (-1) (V a 0) d)
removeUnusedBindings _ = Nothing

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
