{-# language LambdaCase #-}

module Dhall.Traverse ( subExpressions ) where

import Dhall.Core


-- | A traversal over the immediate sub-expressions of an expression.
subExpressions
  :: Applicative f
  => ( Expr s a -> f ( Expr s a ) ) -> Expr s a -> f ( Expr s a )
subExpressions f = \case
  Lam a b c ->
    Lam a <$> f b <*> f c

  Pi a b c ->
    Pi a <$> f b <*> f c

  App a b ->
    App <$> f a <*> f b

  Let a b c d ->
    Let a <$> traverse f b <*> f c <*> f d

  Annot a b ->
    Annot <$> f a <*> f b

  BoolAnd a b ->
    BoolAnd <$> f a <*> f b

  BoolOr a b ->
    BoolOr <$> f a <*> f b

  BoolEQ a b ->
    BoolEQ <$> f a <*> f b

  BoolNE a b ->
    BoolNE <$> f a <*> f b

  BoolIf a b c ->
    BoolIf <$> f a <*> f b <*> f c

  NaturalPlus a b ->
    NaturalPlus <$> f a <*> f b

  NaturalTimes a b ->
    NaturalTimes <$> f a <*> f b

  TextLit (Chunks a b) ->
    TextLit
      <$>
        ( Chunks
            <$> traverse ( \(x,y) -> (,) <$> pure x <*> f y ) a <*> pure b
        )

  TextAppend a b ->
    TextAppend <$> f a <*> f b

  ListLit a b ->
    ListLit <$> traverse f a <*> traverse f b

  ListAppend a b ->
    ListAppend <$> f a <*> f b

  OptionalLit a b ->
    OptionalLit <$> f a <*> traverse f b

  Some a ->
    Some <$> f a

  Record a ->
    Record <$> traverse f a

  RecordLit a ->
    RecordLit <$> traverse f a

  Union a ->
    Union <$> traverse f a

  UnionLit a b c ->
    UnionLit a <$> f b <*> traverse f c

  Combine a b ->
    Combine <$> f a <*> f b

  Prefer a b ->
    Prefer <$> f a <*> f b

  Merge a b t ->
    Merge <$> f a <*> f b <*> traverse f t

  Constructors a ->
    Constructors <$> f a

  Field a b ->
    Field <$> f a <*> pure b

  Note a b ->
    Note a <$> f b

  ImportAlt l r ->
    ImportAlt <$> f l <*> f r

  Const c ->
    pure ( Const c )

  Var v ->
    pure ( Var v )

  Bool ->
    pure Bool

  BoolLit b ->
    pure ( BoolLit b )

  Natural ->
    pure Natural

  NaturalLit n ->
    pure ( NaturalLit n )

  NaturalFold ->
    pure NaturalFold

  NaturalBuild ->
    pure NaturalBuild

  NaturalIsZero ->
    pure NaturalIsZero

  NaturalEven ->
    pure NaturalEven

  NaturalOdd ->
    pure NaturalOdd

  NaturalToInteger ->
    pure NaturalToInteger

  NaturalShow ->
    pure NaturalShow

  Integer ->
    pure Integer

  IntegerLit n ->
    pure ( IntegerLit n )

  IntegerShow ->
    pure IntegerShow

  IntegerToDouble ->
    pure IntegerToDouble

  Double ->
    pure Double

  DoubleLit n ->
    pure ( DoubleLit n )

  DoubleShow ->
    pure DoubleShow

  Text ->
    pure Text

  List ->
    pure List

  ListBuild ->
    pure ListBuild

  ListFold ->
    pure ListFold

  ListLength ->
    pure ListLength

  ListHead ->
    pure ListHead

  ListLast ->
    pure ListLast

  ListIndexed ->
    pure ListIndexed

  ListReverse ->
    pure ListReverse

  Optional ->
    pure Optional

  None ->
    pure None

  OptionalFold ->
    pure OptionalFold

  OptionalBuild ->
    pure OptionalBuild

  CombineTypes a b ->
    CombineTypes <$> f a <*> f b

  Project a b ->
    Project <$> f a <*> pure b

  Embed a ->
    pure ( Embed a )
