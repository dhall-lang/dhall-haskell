{-| This module provides optics for the core syntax types.
-}

module Dhall.Syntax.Optics (
      subExpressions
    , subExpressionsWith
    , unsafeSubExpressions
    , chunkExprs
    , bindingExprs
    , recordFieldExprs
    , functionBindingExprs
    ) where

import Dhall.Syntax
    ( Binding (..)
    , Expr (..)
    , FunctionBinding (..)
    , RecordField (..)
    , chunkExprs
    , unsafeSubExpressions
    )

-- | A traversal over the immediate sub-expressions of an expression.
subExpressions
    :: Applicative f => (Expr s a -> f (Expr s a)) -> Expr s a -> f (Expr s a)
subExpressions = subExpressionsWith (pure . Embed)
{-# INLINABLE subExpressions #-}

{-| A traversal over the immediate sub-expressions of an expression which
    allows mapping embedded values
-}
subExpressionsWith
    :: Applicative f => (a -> f (Expr s b)) -> (Expr s a -> f (Expr s b)) -> Expr s a -> f (Expr s b)
subExpressionsWith h _ (Embed a) = h a
subExpressionsWith _ f (Note a b) = Note a <$> f b
subExpressionsWith _ f (Let a b) = Let <$> bindingExprs f a <*> f b
subExpressionsWith _ f (Record a) = Record <$> traverse (recordFieldExprs f) a
subExpressionsWith _ f (RecordLit a) = RecordLit <$> traverse (recordFieldExprs f) a
subExpressionsWith _ f (Lam cs fb e) = Lam cs <$> functionBindingExprs f fb <*> f e
subExpressionsWith _ f (Field a b) = Field <$> f a <*> pure b
subExpressionsWith _ f expression = unsafeSubExpressions f expression
{-# INLINABLE subExpressionsWith #-}

{-| Traverse over the immediate 'Expr' children in a 'Binding'.
-}
bindingExprs
  :: (Applicative f)
  => (Expr s a -> f (Expr s b))
  -> Binding s a -> f (Binding s b)
bindingExprs f (Binding s0 n s1 t s2 v) =
  Binding
    <$> pure s0
    <*> pure n
    <*> pure s1
    <*> traverse (traverse f) t
    <*> pure s2
    <*> f v
{-# INLINABLE bindingExprs #-}

{-| Traverse over the immediate 'Expr' children in a 'RecordField'.
-}
recordFieldExprs
    :: Applicative f
    => (Expr s a -> f (Expr s b))
    -> RecordField s a -> f (RecordField s b)
recordFieldExprs f (RecordField s0 e s1 s2) =
    RecordField
        <$> pure s0
        <*> f e
        <*> pure s1
        <*> pure s2
{-# INLINABLE recordFieldExprs #-}

{-| Traverse over the immediate 'Expr' children in a 'FunctionBinding'.
-}
functionBindingExprs
    :: Applicative f
    => (Expr s a -> f (Expr s b))
    -> FunctionBinding s a -> f (FunctionBinding s b)
functionBindingExprs f (FunctionBinding s0 label s1 s2 type_) =
    FunctionBinding
        <$> pure s0
        <*> pure label
        <*> pure s1
        <*> pure s2
        <*> f type_
{-# INLINABLE functionBindingExprs #-}
