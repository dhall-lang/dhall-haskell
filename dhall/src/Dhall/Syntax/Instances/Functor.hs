{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Functor () where

import {-# SOURCE #-} Dhall.Syntax.Operations (unsafeSubExpressions)
import                Dhall.Syntax.Types

import qualified Lens.Family as Lens

deriving instance Functor (Binding s)
deriving instance Functor (Chunks s)
deriving instance Functor (PreferAnnotation s)
deriving instance Functor (RecordField s)
deriving instance Functor (FunctionBinding s)
deriving instance Functor FieldSelection

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (Expr s) where
  fmap f (Embed a) = Embed (f a)
  fmap f (Let b e2) = Let (fmap f b) (fmap f e2)
  fmap f (Note s e1) = Note s (fmap f e1)
  fmap f (Record a) = Record $ fmap f <$> a
  fmap f (RecordLit a) = RecordLit $ fmap f <$> a
  fmap f (Lam cs fb e) = Lam cs (f <$> fb) (f <$> e)
  fmap f (Field a b) = Field (f <$> a) b
  fmap f expression = Lens.over unsafeSubExpressions (fmap f) expression
  {-# INLINABLE fmap #-}
