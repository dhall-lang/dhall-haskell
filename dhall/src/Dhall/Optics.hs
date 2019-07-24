{-| This module contains some useful utilities copy-and-pasted from the @lens@
    library to avoid a dependency which are used internally and also re-exported
    for convenience
-}

module Dhall.Optics
    ( -- * Utilities
      rewriteOf
    , transformOf
    , rewriteMOf
    , transformMOf
    , mapMOf
    -- , subExpressions
    ) where

import Control.Applicative (WrappedMonad(..))
import Data.Profunctor.Unsafe ((#.))
import Lens.Family (ASetter, LensLike, over)
import Dhall.Core (Expr(..), Binding(..), Chunks(..))

-- | Identical to @"Control.Lens".`Control.Lens.rewriteOf`@
rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go
  where
    go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.transformOf`@
transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go
  where
    go = f . over l go
{-# INLINE transformOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.rewriteMOf`@
rewriteMOf
    :: Monad m
    => LensLike (WrappedMonad m) a b a b -> (b -> m (Maybe a)) -> a -> m b
rewriteMOf l f = go
  where
    go = transformMOf l (\x -> f x >>= maybe (return x) go)
{-# INLINE rewriteMOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.transformMOf`@
transformMOf
    :: Monad m => LensLike (WrappedMonad m) a b a b -> (b -> m b) -> a -> m b
transformMOf l f = go
  where
    go t = mapMOf l go t >>= f
{-# INLINE transformMOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.mapMOf`@
mapMOf :: LensLike (WrappedMonad m) s t a b -> (a -> m b) -> s -> m t
mapMOf l cmd = unwrapMonad #. l (WrapMonad #. cmd)
{-# INLINE mapMOf #-}

-- -- | A traversal over the immediate sub-expressions of an expression.
-- subExpressions :: Applicative f => (Expr s a -> f (Expr s a)) -> Expr s a -> f (Expr s a)
-- subExpressions _ (Const c) = pure (Const c)
-- subExpressions _ (Var v) = pure (Var v)
-- subExpressions f (Lam a b c) = Lam a <$> f b <*> f c
-- subExpressions f (Pi a b c) = Pi a <$> f b <*> f c
-- subExpressions f (App a b) = App <$> f a <*> f b
-- subExpressions f (Let as b) = Let <$> traverse g as <*> f b where
--                                 g (Binding c d e) = Binding c <$> traverse f d <*> f e
-- subExpressions f (Annot a b) = Annot <$> f a <*> f b
-- subExpressions _ Bool = pure Bool
-- subExpressions _ (BoolLit b) = pure (BoolLit b)
-- subExpressions f (BoolAnd a b) = BoolAnd <$> f a <*> f b
-- subExpressions f (BoolOr a b) = BoolOr <$> f a <*> f b
-- subExpressions f (BoolEQ a b) = BoolEQ <$> f a <*> f b
-- subExpressions f (BoolNE a b) = BoolNE <$> f a <*> f b
-- subExpressions f (BoolIf a b c) = BoolIf <$> f a <*> f b <*> f c
-- subExpressions _ Natural = pure Natural
-- subExpressions _ (NaturalLit n) = pure (NaturalLit n)
-- subExpressions _ NaturalFold = pure NaturalFold
-- subExpressions _ NaturalBuild = pure NaturalBuild
-- subExpressions _ NaturalIsZero = pure NaturalIsZero
-- subExpressions _ NaturalEven = pure NaturalEven
-- subExpressions _ NaturalOdd = pure NaturalOdd
-- subExpressions _ NaturalToInteger = pure NaturalToInteger
-- subExpressions _ NaturalShow = pure NaturalShow
-- subExpressions f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
-- subExpressions f (NaturalTimes a b) = NaturalTimes <$> f a <*> f b
-- subExpressions _ Integer = pure Integer
-- subExpressions _ (IntegerLit n) = pure (IntegerLit n)
-- subExpressions _ IntegerShow = pure IntegerShow
-- subExpressions _ IntegerToDouble = pure IntegerToDouble
-- subExpressions _ Double = pure Double
-- subExpressions _ (DoubleLit n) = pure (DoubleLit n)
-- subExpressions _ DoubleShow = pure DoubleShow
-- subExpressions _ Text = pure Text
-- subExpressions f (TextLit chunks) = TextLit <$> chunkExprs f chunks
-- subExpressions f (TextAppend a b) = TextAppend <$> f a <*> f b
-- subExpressions _ TextShow = pure TextShow
-- subExpressions _ List = pure List
-- subExpressions f (ListLit a b) = ListLit <$> traverse f a <*> traverse f b
-- subExpressions f (ListAppend a b) = ListAppend <$> f a <*> f b
-- subExpressions _ ListBuild = pure ListBuild
-- subExpressions _ ListFold = pure ListFold
-- subExpressions _ ListLength = pure ListLength
-- subExpressions _ ListHead = pure ListHead
-- subExpressions _ ListLast = pure ListLast
-- subExpressions _ ListIndexed = pure ListIndexed
-- subExpressions _ ListReverse = pure ListReverse
-- subExpressions _ Optional = pure Optional
-- subExpressions f (Some a) = Some <$> f a
-- subExpressions _ None = pure None
-- subExpressions _ OptionalFold = pure OptionalFold
-- subExpressions _ OptionalBuild = pure OptionalBuild
-- subExpressions f (Record a) = Record <$> traverse f a
-- subExpressions f ( RecordLit a ) = RecordLit <$> traverse f a
-- subExpressions f (Union a) = Union <$> traverse (traverse f) a
-- subExpressions f (Combine a b) = Combine <$> f a <*> f b
-- subExpressions f (CombineTypes a b) = CombineTypes <$> f a <*> f b
-- subExpressions f (Prefer a b) = Prefer <$> f a <*> f b
-- subExpressions f (Merge a b t) = Merge <$> f a <*> f b <*> traverse f t
-- subExpressions f (ToMap a t) = ToMap <$> f a <*> traverse f t
-- subExpressions f (Project a b) = Project <$> f a <*> traverse f b
-- subExpressions f (ImportAlt l r b) = ImportAlt <$> f l <*> f r <*> pure b
-- subExpressions _ (EmbedImport a) = pure (Embed a)

-- chunkExprs
--   :: Applicative f
--   => (Expr s a -> f (Expr t b))
--   -> Chunks s a -> f (Chunks t b)
-- chunkExprs f (Chunks chunks final) =
--   flip Chunks final <$> traverse (traverse f) chunks
