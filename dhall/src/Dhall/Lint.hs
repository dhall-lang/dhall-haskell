-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    , denote
    , removeLetInLet
    , removeUnusedBindings
    , optionalLitToSomeNone
    ) where

import Control.Monad (mplus)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup ((<>))
import Dhall.Core (Binding(..), Expr(..), Import, Var(..), Chunks(..))
import Lens.Family (ASetter, over)
import Unsafe.Coerce (unsafeCoerce)

import qualified Dhall.Core

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings with 'removeLetInLet'.
    * consolidates nested @let@ bindings to use a multiple-@let@ binding with 'removeUnusedBindings'.
    * switches legacy @List@-like @Optional@ literals to use @Some@ / @None@ instead with 'optionalLitToSomeNone'
-}
lint :: Expr s Import -> Expr t Import
lint =
  unsafeCoerce .
  rewriteOf
    subExpressions
    ( \e ->
                removeLetInLet e
        `mplus` removeUnusedBindings e
        `mplus` optionalLitToSomeNone e
        `mplus` denote e
    )

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

denote :: Expr s a -> Maybe (Expr s a)
denote (Note _ t) = Just t
denote _          = Nothing


optionalLitToSomeNone :: Expr s a -> Maybe (Expr s a)
optionalLitToSomeNone (OptionalLit _ (Just b)) = Just (Some b)
optionalLitToSomeNone (OptionalLit a Nothing) = Just (App None a)
optionalLitToSomeNone _ = Nothing


rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where go = transformOf l (\x -> maybe x go (f x))

transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go where go = f . over l go

-- | A traversal over the immediate sub-expressions of an expression.
subExpressions :: Applicative f => (Expr s a -> f (Expr s a)) -> Expr s a -> f (Expr s a)
subExpressions _ (Const c) = pure (Const c)
subExpressions _ (Var v) = pure (Var v)
subExpressions f (Lam a b c) = Lam a <$> f b <*> f c
subExpressions f (Pi a b c) = Pi a <$> f b <*> f c
subExpressions f (App a b) = App <$> f a <*> f b
subExpressions f (Let as b) = Let <$> traverse g as <*> f b
  where
    g (Binding c d e) = Binding c <$> traverse f d <*> f e
subExpressions f (Annot a b) = Annot <$> f a <*> f b
subExpressions _ Bool = pure Bool
subExpressions _ (BoolLit b) = pure (BoolLit b)
subExpressions f (BoolAnd a b) = BoolAnd <$> f a <*> f b
subExpressions f (BoolOr a b) = BoolOr <$> f a <*> f b
subExpressions f (BoolEQ a b) = BoolEQ <$> f a <*> f b
subExpressions f (BoolNE a b) = BoolNE <$> f a <*> f b
subExpressions f (BoolIf a b c) = BoolIf <$> f a <*> f b <*> f c
subExpressions _ Natural = pure Natural
subExpressions _ (NaturalLit n) = pure (NaturalLit n)
subExpressions _ NaturalFold = pure NaturalFold
subExpressions _ NaturalBuild = pure NaturalBuild
subExpressions _ NaturalIsZero = pure NaturalIsZero
subExpressions _ NaturalEven = pure NaturalEven
subExpressions _ NaturalOdd = pure NaturalOdd
subExpressions _ NaturalToInteger = pure NaturalToInteger
subExpressions _ NaturalShow = pure NaturalShow
subExpressions f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
subExpressions f (NaturalTimes a b) = NaturalTimes <$> f a <*> f b
subExpressions _ Integer = pure Integer
subExpressions _ (IntegerLit n) = pure (IntegerLit n)
subExpressions _ IntegerShow = pure IntegerShow
subExpressions _ IntegerToDouble = pure IntegerToDouble
subExpressions _ Double = pure Double
subExpressions _ (DoubleLit n) = pure (DoubleLit n)
subExpressions _ DoubleShow = pure DoubleShow
subExpressions _ Text = pure Text
subExpressions f (TextLit (Chunks a b)) =
    TextLit <$> (Chunks <$> traverse (traverse f) a <*> pure b)
subExpressions f (TextAppend a b) = TextAppend <$> f a <*> f b
subExpressions _ TextShow = pure TextShow
subExpressions _ List = pure List
subExpressions f (ListLit a b) = ListLit <$> traverse f a <*> traverse f b
subExpressions f (ListAppend a b) = ListAppend <$> f a <*> f b
subExpressions _ ListBuild = pure ListBuild
subExpressions _ ListFold = pure ListFold
subExpressions _ ListLength = pure ListLength
subExpressions _ ListHead = pure ListHead
subExpressions _ ListLast = pure ListLast
subExpressions _ ListIndexed = pure ListIndexed
subExpressions _ ListReverse = pure ListReverse
subExpressions _ Optional = pure Optional
subExpressions f (OptionalLit a b) = OptionalLit <$> f a <*> traverse f b
subExpressions f (Some a) = Some <$> f a
subExpressions _ None = pure None
subExpressions _ OptionalFold = pure OptionalFold
subExpressions _ OptionalBuild = pure OptionalBuild
subExpressions f (Record a) = Record <$> traverse f a
subExpressions f ( RecordLit a ) = RecordLit <$> traverse f a
subExpressions f (Union a) = Union <$> traverse (traverse f) a
subExpressions f (UnionLit a b c) =
    UnionLit a <$> f b <*> traverse (traverse f) c
subExpressions f (Combine a b) = Combine <$> f a <*> f b
subExpressions f (CombineTypes a b) = CombineTypes <$> f a <*> f b
subExpressions f (Prefer a b) = Prefer <$> f a <*> f b
subExpressions f (Merge a b t) = Merge <$> f a <*> f b <*> traverse f t
subExpressions f (Field a b) = Field <$> f a <*> pure b
subExpressions f (Project a b) = Project <$> f a <*> pure b
subExpressions f (Note a b) = Note a <$> f b
subExpressions f (ImportAlt l r) = ImportAlt <$> f l <*> f r
subExpressions _ (Embed a) = pure (Embed a)
