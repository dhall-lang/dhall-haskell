{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Applicative () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Expr
import Dhall.Syntax.FunctionBinding
import Dhall.Syntax.Instances.Functor ()
import Dhall.Syntax.Operations
import Dhall.Syntax.RecordField

import qualified Lens.Micro as Lens

instance Applicative (Expr s) where
    pure = Embed

    expression <*> k = case expression of
        Embed a     -> a <$> k
        Let a b     -> Let (adaptBinding a) (b <*> k)
        Note a b    -> Note a (b <*> k)
        Record a    -> Record $ adaptRecordField <$> a
        RecordLit a -> RecordLit $ adaptRecordField <$> a
        Lam cs a b  -> Lam cs (adaptFunctionBinding a) (b <*> k)
        Field a b   -> Field (a <*> k) b
        _ -> Lens.over unsafeSubExpressions (<*> k) expression
      where
        adaptRecordField (RecordField s0 e s1 s2) =
            RecordField s0 (e <*> k) s1 s2

        adaptBinding (Binding src0 c src1 d src2 e) =
            Binding src0 c src1 (fmap adaptBindingAnnotation d) src2 (e <*> k)

        adaptFunctionBinding (FunctionBinding src0 label src1 src2 type_) =
            FunctionBinding src0 label src1 src2 (type_ <*> k)

        adaptBindingAnnotation (src3, f) = (src3, f <*> k)
