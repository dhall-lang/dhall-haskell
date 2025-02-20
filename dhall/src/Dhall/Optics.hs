{-| This module contains some useful utilities copy-and-pasted from the @lens@
    library to avoid a dependency which are used internally and also re-exported
    for convenience
-}

{-# LANGUAGE RankNTypes #-}

module Dhall.Optics {-# DEPRECATED "Use the definitions from Lens.Micro of the microlens package directly." #-}
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
import Data.Monoid                (Any (..))
import Lens.Micro                 (ASetter, LensLike, Traversal, SimpleGetter)
import Lens.Micro.Internal        ((#.))

import qualified Lens.Micro

-- | Identical to @"Control.Lens.Getter".`Control.Lens.Getter.Getting`@
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Identical to @"Control.Lens.Type".`Control.Lens.Type.Optic`@
type Optic p f s t a b = p a (f b) -> p s (f t)

-- | Identical to @"Control.Lens.Type".`Control.Lens.Type.Optic'`@
type Optic' p f s a = Optic p f s s a a

-- | Identical to @"Control.Lens".`Control.Lens.anyOf`@
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf = Lens.Micro.anyOf
{-# INLINE anyOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.rewriteOf`@
rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf = Lens.Micro.rewriteOf
{-# INLINE rewriteOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.transformOf`@
transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf = Lens.Micro.transformOf
{-# INLINE transformOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.rewriteMOf`@
rewriteMOf
    :: Monad m
    => LensLike (WrappedMonad m) a b a b -> (b -> m (Maybe a)) -> a -> m b
rewriteMOf = Lens.Micro.rewriteMOf
{-# INLINE rewriteMOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.transformMOf`@
transformMOf
    :: Monad m => LensLike (WrappedMonad m) a b a b -> (b -> m b) -> a -> m b
transformMOf = Lens.Micro.transformMOf
{-# INLINE transformMOf #-}

-- | Identical to @"Control.Lens".`Control.Lens.mapMOf`@
mapMOf :: LensLike (WrappedMonad m) s t a b -> (a -> m b) -> s -> m t
mapMOf = Lens.Micro.mapMOf
{-# INLINE mapMOf #-}

-- | Identical to @"Control.Lens.Plated".`Control.Lens.Plated.cosmosOf`@
cosmosOf :: Traversal s t s t -> Traversal s t s b
cosmosOf = Lens.Micro.cosmosOf
{-# INLINE cosmosOf #-}

-- | Identical to @"Control.Lens.Getter".`Control.Lens.Getter.to`@
to :: (s -> a) -> SimpleGetter s a
to = Lens.Micro.to
{-# INLINE to #-}

-- | Identical to @"Control.Lens.Fold".`Control.Lens.Fold.foldOf`@
foldOf :: Getting a s a -> s -> a
foldOf l = getConst #. l Const
{-# INLINE foldOf #-}
