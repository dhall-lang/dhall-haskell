{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Bifunctor () where

import Data.Bifunctor          (Bifunctor (..))
import Dhall.Syntax.Operations
import Dhall.Syntax.Types

import qualified Lens.Family as Lens

instance Bifunctor Binding where
    first k (Binding src0 a src1 b src2 c) =
        Binding (fmap k src0) a (fmap k src1) (fmap adapt0 b) (fmap k src2) (first k c)
      where
        adapt0 (src3, d) = (fmap k src3, first k d)

    second = fmap

instance Bifunctor PreferAnnotation where
    first _  PreferFromSource      = PreferFromSource
    first f (PreferFromWith e    ) = PreferFromWith (first f e)
    first _  PreferFromCompletion  = PreferFromCompletion

    second = fmap

instance Bifunctor RecordField where
    first k (RecordField s0 value' s1 s2) =
        RecordField (k <$> s0) (first k value') (k <$> s1) (k <$> s2)
    second = fmap

instance Bifunctor FunctionBinding where
    first k (FunctionBinding src0 label src1 src2 type_) =
        FunctionBinding (k <$> src0) label (k <$> src1) (k <$> src2) (first k type_)

    second = fmap

instance Bifunctor Expr where
    first k (Note a b   ) = Note (k a) (first k b)
    first _ (Embed a    ) = Embed a
    first k (Let a b    ) = Let (first k a) (first k b)
    first k (Record a   ) = Record $ first k <$> a
    first k (RecordLit a) = RecordLit $ first k <$> a
    first k (Lam cs a b ) = Lam cs (first k a) (first k b)
    first k (Field a b  ) = Field (first k a) (k <$> b)
    first k  expression  = Lens.over unsafeSubExpressions (first k) expression

    second = fmap
