{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Monad () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Expr
import Dhall.Syntax.FunctionBinding
import Dhall.Syntax.Instances.Applicative ()
import Dhall.Syntax.Operations
import Dhall.Syntax.RecordField

import qualified Lens.Family as Lens

instance Monad (Expr s) where
    return = pure

    expression >>= k = case expression of
        Embed a     -> k a
        Let a b     -> Let (adaptBinding a) (b >>= k)
        Note a b    -> Note a (b >>= k)
        Record a    -> Record $ bindRecordKeyValues <$> a
        RecordLit a -> RecordLit $ bindRecordKeyValues <$> a
        Lam cs a b  -> Lam cs (adaptFunctionBinding a) (b >>= k)
        Field a b   -> Field (a >>= k) b
        _ -> Lens.over unsafeSubExpressions (>>= k) expression
      where
        bindRecordKeyValues (RecordField s0 e s1 s2) =
            RecordField s0 (e >>= k) s1 s2

        adaptBinding (Binding src0 c src1 d src2 e) =
            Binding src0 c src1 (fmap adaptBindingAnnotation d) src2 (e >>= k)

        adaptFunctionBinding (FunctionBinding src0 label src1 src2 type_) =
            FunctionBinding src0 label src1 src2 (type_ >>= k)

        adaptBindingAnnotation (src3, f) = (src3, f >>= k)
