-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    , removeLetInLet
    , removeUnusedBindings
    , optionalLitToSomeNone
    ) where

import Control.Monad (mplus)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Dhall.Core (Binding(..), Expr(..), Import, Var(..), subExpressions)
import Lens.Family (ASetter, over)

import qualified Dhall.Core

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeLetInLet'.
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeUnusedBindings'.
    * switches legacy @List@-like @Optional@ literals to use @Some@ / @None@ instead with 'optionalLitToSomeNone'
-}
lint :: Expr s Import -> Expr t Import
lint =
  rewriteOf
    subExpressions
    ( \e ->
                removeLetInLet e
        `mplus` removeUnusedBindings e
        `mplus` optionalLitToSomeNone e
    )
    . Dhall.Core.denote

removeLetInLet :: Eq a => Expr s a -> Maybe (Expr s a)
removeLetInLet (Let a (Let b c)) = Just (Let (a <> b) c)
removeLetInLet _ = Nothing

removeUnusedBindings :: Eq a => Expr s a -> Maybe (Expr s a)
removeUnusedBindings (Let (Binding a _ _ :| []) d)
    | not (V a 0 `Dhall.Core.freeIn` d) =
        Just d
    | otherwise =
        Nothing
removeUnusedBindings (Let (Binding a _ _ :| (l : ls)) d)
    | not (V a 0 `Dhall.Core.freeIn` e) =
        Just e
    | otherwise =
        Nothing
  where
    e = Let (l :| ls) d
removeUnusedBindings _ = Nothing


optionalLitToSomeNone :: Expr s a -> Maybe (Expr s a)
optionalLitToSomeNone (OptionalLit _ (Just b)) = Just (Some b)
optionalLitToSomeNone (OptionalLit a Nothing) = Just (App None a)
optionalLitToSomeNone _ = Nothing


rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where go = transformOf l (\x -> maybe x go (f x))

transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go where go = f . over l go
