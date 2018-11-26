-- | This module contains the implementation of the @dhall lint@ command

module Dhall.Lint
    ( -- * Lint
      lint
    ) where

import Dhall.Core (Binding(..), Chunks(..), Expr(..), Import, Var(..))
import Data.Semigroup ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import Dhall.TypeCheck (X(..))

import qualified Dhall.Core

{-| Automatically improve a Dhall expression

    Currently this:

    * removes unused @let@ bindings
    * consolidates nested @let@ bindings to use a multiple-@let@ binding
    * switches legacy @List@-like @Optional@ literals to use @Some@ / @None@ instead
-}
lint :: Expr s Import -> Expr t Import
lint expression = loop (Dhall.Core.denote expression)
  where
    loop (Const a) =
        Const a
    loop (Var a) =
        Var a
    loop (Lam a b c) = Lam a b' c'
      where
        b' = loop b
        c' = loop c
    loop (Pi a b c) = Pi a b' c'
      where
        b' = loop b
        c' = loop c
    loop (App a b) = App a' b'
      where
        a' = loop a
        b' = loop b

    -- Consolidate nested `let` expresssions
    loop (Let a (Let b c)) = loop (Let (a <> b) c)

    -- Remove unused bindings
    loop (Let (Binding a b c :| []) d)
        | not (V a 0 `Dhall.Core.freeIn` d') =
            d'
        | otherwise =
            Let (Binding a b' c' :| [])  d'
      where
        b' = fmap loop b
        c' =      loop c

        d' = loop d
    loop (Let (Binding a b c :| (l : ls)) d)
        | not (V a 0 `Dhall.Core.freeIn` Let (l' :| ls') d') =
            Let (l' :| ls') d'
        | otherwise =
            Let (Binding a b' c' :| (l' : ls'))  d'
      where
        b' = fmap loop b
        c' =      loop c

        Let (l' :| ls') d' = loop (Let (l :| ls) d)

    loop (Annot a b) =
        Annot a' b'
      where
        a' = loop a
        b' = loop b
    loop Bool =
        Bool
    loop (BoolLit a) =
        BoolLit a
    loop (BoolAnd a b) =
        BoolAnd a' b'
      where
        a' = loop a
        b' = loop b
    loop (BoolOr a b) =
        BoolOr a' b'
      where
        a' = loop a
        b' = loop b
    loop (BoolEQ a b) =
        BoolEQ a' b'
      where
        a' = loop a
        b' = loop b
    loop (BoolNE a b) =
        BoolNE a' b'
      where
        a' = loop a
        b' = loop b
    loop (BoolIf a b c) =
        BoolIf a' b' c'
      where
        a' = loop a
        b' = loop b
        c' = loop c
    loop Natural =
        Natural
    loop (NaturalLit a) =
        NaturalLit a
    loop NaturalFold =
        NaturalFold
    loop NaturalBuild =
        NaturalBuild
    loop NaturalIsZero =
        NaturalIsZero
    loop NaturalEven =
        NaturalEven
    loop NaturalOdd =
        NaturalOdd
    loop NaturalToInteger =
        NaturalToInteger
    loop NaturalShow =
        NaturalShow
    loop (NaturalPlus a b) =
        NaturalPlus a' b'
      where
        a' = loop a
        b' = loop b
    loop (NaturalTimes a b) =
        NaturalTimes a' b'
      where
        a' = loop a
        b' = loop b
    loop Integer =
        Integer
    loop (IntegerLit a) =
        IntegerLit a
    loop IntegerShow =
        IntegerShow
    loop IntegerToDouble =
        IntegerToDouble
    loop Double =
        Double
    loop (DoubleLit a) =
        DoubleLit a
    loop DoubleShow =
        DoubleShow
    loop Text =
        Text
    loop (TextLit (Chunks a b)) =
        TextLit (Chunks a' b)
      where
        a' = fmap (fmap loop) a
    loop (TextAppend a b) =
        TextAppend a' b'
      where
        a' = loop a
        b' = loop b
    loop List =
        List
    loop (ListLit a b) =
        ListLit a' b'
      where
        a' = fmap loop a
        b' = fmap loop b
    loop (ListAppend a b) =
        ListAppend a' b'
      where
        a' = loop a
        b' = loop b
    loop ListBuild =
        ListBuild
    loop ListFold =
        ListFold
    loop ListLength =
        ListLength
    loop ListHead =
        ListHead
    loop ListLast =
        ListLast
    loop ListIndexed =
        ListIndexed
    loop ListReverse =
        ListReverse
    loop Optional =
        Optional
    loop (Some a) =
        Some a'
      where
        a' = loop a
    loop None =
        None
    loop (OptionalLit _ (Just b)) =
        loop (Some b)
    loop (OptionalLit a Nothing) =
        loop (App None a)
    loop OptionalFold =
        OptionalFold
    loop OptionalBuild =
        OptionalBuild
    loop (Record a) =
        Record a'
      where
        a' = fmap loop a
    loop (RecordLit a) =
        RecordLit a'
      where
        a' = fmap loop a
    loop (Union a) =
        Union a'
      where
        a' = fmap loop a
    loop (UnionLit a b c) =
        UnionLit a b' c'
      where
        b' =      loop b
        c' = fmap loop c
    loop (Combine a b) =
        Combine a' b'
      where
        a' = loop a
        b' = loop b
    loop (CombineTypes a b) =
        CombineTypes a' b'
      where
        a' = loop a
        b' = loop b
    loop (Prefer a b) =
        Prefer a' b'
      where
        a' = loop a
        b' = loop b
    loop (Merge a b c) =
        Merge a' b' c'
      where
        a' =      loop a
        b' =      loop b
        c' = fmap loop c
    loop (Constructors a) =
        Constructors a'
      where
        a' = loop a
    loop (Field a b) =
        Field a' b
      where
        a' = loop a
    loop (Project a b) =
        Project a' b
      where
        a' = loop a
    loop (Note a _) =
        absurd a
    loop (ImportAlt a b) =
        ImportAlt a' b'
      where
        a' = loop a
        b' = loop b
    loop (Embed a) =
        Embed a
