{-| This module contains some useful utilities copy-and-pasted from the @lens@
    library to avoid a dependency which are used internally and also re-exported
    for convenience
-}

{-# LANGUAGE RankNTypes #-}

module Dhall.Optics
    ( Optic
    , Optic'
       -- * Utilities
    , anyOf
    , rewriteOf
    , transformOf
    , rewriteMOf
    , transformMOf
    , mapMOf
    , cosmosOf
    , to
    , foldOf
    ) where

import Control.Applicative        (Const (..), WrappedMonad (..))
import Data.Coerce                (coerce)
import Data.Monoid                (Any (..))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Profunctor            (Profunctor (dimap))
import Lens.Micro                 (ASetter, LensLike, Traversal)
import Lens.Micro.Internal        (foldMapOf, (#.))

import qualified Lens.Micro

-- | Identical to @"Control.Lens.Getter".`Control.Lens.Getter.Getting`@
{-# DEPRECATED Getting "Use Lens.Micro.Getting directly." #-}
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Identical to @"Control.Lens.Type".`Control.Lens.Type.Optic`@
type Optic p f s t a b = p a (f b) -> p s (f t)

-- | Identical to @"Control.Lens.Type".`Control.Lens.Type.Optic'`@
type Optic' p f s a = Optic p f s s a a

-- | Identical to @"Control.Lens".`Control.Lens.anyOf`@
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny #. foldMapOf l (Any #. f)
{-# INLINE anyOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.rewriteOf`@
{-# DEPRECATED rewriteOf "Use Lens.Micro.rewriteOf directly." #-}
rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf = Lens.Micro.rewriteOf
{-# INLINE rewriteOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.transformOf`@
{-# DEPRECATED transformOf "Use Lens.Micro.transformOf directly." #-}
transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf = Lens.Micro.transformOf
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
mapMOf = coerce
{-# INLINE mapMOf #-}

-- | Identical to @"Control.Lens.Plated".`Control.Lens.Plated.cosmosOf`@
cosmosOf :: Traversal s t s t -> Traversal s t s b
cosmosOf d f s = f s *> d (cosmosOf d f) s
{-# INLINE cosmosOf #-}

-- | Identical to @"Control.Lens.Getter".`Control.Lens.Getter.to`@
{-# DEPRECATED to "Migrate to Lens.Micro.to or a similar alternative." #-}
to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
to k = dimap k (contramap k)
{-# INLINE to #-}

-- | Identical to @"Control.Lens.Fold".`Control.Lens.Fold.foldOf`@
foldOf :: Getting a s a -> s -> a
foldOf l = getConst #. l Const
{-# INLINE foldOf #-}
