{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Dhall.Syntax.Operations
    ( -- * Optics
      subExpressions
    , subExpressionsWith
    , unsafeSubExpressions

      -- * Handling 'Note's
    , denote
    , renote
    , shallowDenote

      -- * Reserved identifiers
    , reservedIdentifiers
    , reservedKeywords

      -- * Utilities
    , internalError
      -- `shift` should really be in `Dhall.Normalize`, but it's here to avoid a
      -- module cycle
    , shift
    ) where

import Data.HashSet                 (HashSet)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Dhall.Syntax.Binding         (Binding (..), bindingExprs)
import Dhall.Syntax.Chunks          (chunkExprs)
import Dhall.Syntax.Expr
import Dhall.Syntax.FunctionBinding
import Dhall.Syntax.RecordField     (RecordField (..), recordFieldExprs)
import Dhall.Syntax.Types
import Dhall.Syntax.Var
import Unsafe.Coerce                (unsafeCoerce)
import {-# SOURCE #-} Dhall.Pretty.Internal        (ChooseCharacterSet(..))

import qualified Data.HashSet
import qualified Data.Text
import qualified Lens.Micro   as Lens


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

{-| An internal utility used to implement transformations that require changing
    one of the type variables of the `Expr` type

    This utility only works because the implementation is partial, not
    handling the `Let`, `Note`, or `Embed` cases, which need to be handled by
    the caller.
-}
unsafeSubExpressions
    :: Applicative f => (Expr s a -> f (Expr t b)) -> Expr s a -> f (Expr t b)
unsafeSubExpressions _ (Const c) = pure (Const c)
unsafeSubExpressions _ (Var v) = pure (Var v)
unsafeSubExpressions f (Pi cs a b c) = Pi cs a <$> f b <*> f c
unsafeSubExpressions f (App a b) = App <$> f a <*> f b
unsafeSubExpressions f (Annot a b) = Annot <$> f a <*> f b
unsafeSubExpressions _ Bool = pure Bool
unsafeSubExpressions _ (BoolLit b) = pure (BoolLit b)
unsafeSubExpressions f (BoolAnd a b) = BoolAnd <$> f a <*> f b
unsafeSubExpressions f (BoolOr a b) = BoolOr <$> f a <*> f b
unsafeSubExpressions f (BoolEQ a b) = BoolEQ <$> f a <*> f b
unsafeSubExpressions f (BoolNE a b) = BoolNE <$> f a <*> f b
unsafeSubExpressions f (BoolIf a b c) = BoolIf <$> f a <*> f b <*> f c
unsafeSubExpressions _ Bytes = pure Bytes
unsafeSubExpressions _ (BytesLit a) = pure (BytesLit a)
unsafeSubExpressions _ Natural = pure Natural
unsafeSubExpressions _ (NaturalLit n) = pure (NaturalLit n)
unsafeSubExpressions _ NaturalFold = pure NaturalFold
unsafeSubExpressions _ NaturalBuild = pure NaturalBuild
unsafeSubExpressions _ NaturalIsZero = pure NaturalIsZero
unsafeSubExpressions _ NaturalEven = pure NaturalEven
unsafeSubExpressions _ NaturalOdd = pure NaturalOdd
unsafeSubExpressions _ NaturalToInteger = pure NaturalToInteger
unsafeSubExpressions _ NaturalShow = pure NaturalShow
unsafeSubExpressions _ NaturalSubtract = pure NaturalSubtract
unsafeSubExpressions f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
unsafeSubExpressions f (NaturalTimes a b) = NaturalTimes <$> f a <*> f b
unsafeSubExpressions _ Integer = pure Integer
unsafeSubExpressions _ (IntegerLit n) = pure (IntegerLit n)
unsafeSubExpressions _ IntegerClamp = pure IntegerClamp
unsafeSubExpressions _ IntegerNegate = pure IntegerNegate
unsafeSubExpressions _ IntegerShow = pure IntegerShow
unsafeSubExpressions _ IntegerToDouble = pure IntegerToDouble
unsafeSubExpressions _ Double = pure Double
unsafeSubExpressions _ (DoubleLit n) = pure (DoubleLit n)
unsafeSubExpressions _ DoubleShow = pure DoubleShow
unsafeSubExpressions _ Text = pure Text
unsafeSubExpressions f (TextLit chunks) = TextLit <$> chunkExprs f chunks
unsafeSubExpressions f (TextAppend a b) = TextAppend <$> f a <*> f b
unsafeSubExpressions _ TextReplace = pure TextReplace
unsafeSubExpressions _ TextShow = pure TextShow
unsafeSubExpressions _ Date = pure Date
unsafeSubExpressions _ (DateLiteral a) = pure (DateLiteral a)
unsafeSubExpressions _ DateShow = pure DateShow
unsafeSubExpressions _ Time = pure Time
unsafeSubExpressions _ (TimeLiteral a b) = pure (TimeLiteral a b)
unsafeSubExpressions _ TimeShow = pure TimeShow
unsafeSubExpressions _ TimeZone = pure TimeZone
unsafeSubExpressions _ (TimeZoneLiteral a) = pure (TimeZoneLiteral a)
unsafeSubExpressions _ TimeZoneShow = pure TimeZoneShow
unsafeSubExpressions _ List = pure List
unsafeSubExpressions f (ListLit a b) = ListLit <$> traverse f a <*> traverse f b
unsafeSubExpressions f (ListAppend a b) = ListAppend <$> f a <*> f b
unsafeSubExpressions _ ListBuild = pure ListBuild
unsafeSubExpressions _ ListFold = pure ListFold
unsafeSubExpressions _ ListLength = pure ListLength
unsafeSubExpressions _ ListHead = pure ListHead
unsafeSubExpressions _ ListLast = pure ListLast
unsafeSubExpressions _ ListIndexed = pure ListIndexed
unsafeSubExpressions _ ListReverse = pure ListReverse
unsafeSubExpressions _ Optional = pure Optional
unsafeSubExpressions f (Some a) = Some <$> f a
unsafeSubExpressions _ None = pure None
unsafeSubExpressions f (Union a) = Union <$> traverse (traverse f) a
unsafeSubExpressions f (Combine cs a b c) = Combine cs a <$> f b <*> f c
unsafeSubExpressions f (CombineTypes cs a b) = CombineTypes cs <$> f a <*> f b
unsafeSubExpressions f (Prefer cs a b c) = Prefer cs <$> pure a <*> f b <*> f c
unsafeSubExpressions f (RecordCompletion a b) = RecordCompletion <$> f a <*> f b
unsafeSubExpressions f (Merge a b t) = Merge <$> f a <*> f b <*> traverse f t
unsafeSubExpressions f (ToMap a t) = ToMap <$> f a <*> traverse f t
unsafeSubExpressions f (ShowConstructor a) = ShowConstructor <$> f a
unsafeSubExpressions f (Project a b) = Project <$> f a <*> traverse f b
unsafeSubExpressions f (Assert a) = Assert <$> f a
unsafeSubExpressions f (Equivalent cs a b) = Equivalent cs <$> f a <*> f b
unsafeSubExpressions f (With a b c) = With <$> f a <*> pure b <*> f c
unsafeSubExpressions f (ImportAlt l r) = ImportAlt <$> f l <*> f r
unsafeSubExpressions _ (Let {}) = unhandledConstructor "Let"
unsafeSubExpressions _ (Note {}) = unhandledConstructor "Note"
unsafeSubExpressions _ (Embed {}) = unhandledConstructor "Embed"
unsafeSubExpressions _ (Record {}) = unhandledConstructor "Record"
unsafeSubExpressions _ (RecordLit {}) = unhandledConstructor "RecordLit"
unsafeSubExpressions _ (Lam {}) = unhandledConstructor "Lam"
unsafeSubExpressions _ (Field {}) = unhandledConstructor "Field"
{-# INLINABLE unsafeSubExpressions #-}

unhandledConstructor :: Text -> a
unhandledConstructor constructor =
    internalError
        (   "Dhall.Syntax.unsafeSubExpressions: Unhandled "
        <>  constructor
        <>  " construtor"
        )

-- | Remove all `Note` constructors from an `Expr` (i.e. de-`Note`)
--
-- This also remove CharacterSet annotations.
denote :: Expr s a -> Expr t a
denote = \case
    Note _ b -> denote b
    Let a b -> Let (denoteBinding a) (denote b)
    Embed a -> Embed a
    Combine _ _ b c -> Combine AutoInferCharSet Nothing (denote b) (denote c)
    CombineTypes _ b c -> CombineTypes AutoInferCharSet (denote b) (denote c)
    Prefer _ a b c -> Lens.over unsafeSubExpressions denote $ Prefer AutoInferCharSet a b c
    Record a -> Record $ denoteRecordField <$> a
    RecordLit a -> RecordLit $ denoteRecordField <$> a
    Lam _ a b -> Lam AutoInferCharSet (denoteFunctionBinding a) (denote b)
    Pi _ t a b -> Pi AutoInferCharSet t (denote a) (denote b)
    Field a (FieldSelection _ b _) -> Field (denote a) (FieldSelection Nothing b Nothing)
    Equivalent _ a b -> Equivalent AutoInferCharSet (denote a) (denote b)
    expression -> Lens.over unsafeSubExpressions denote expression
  where
    denoteRecordField (RecordField _ e _ _) = RecordField Nothing (denote e) Nothing Nothing

    denoteBinding (Binding _ c _ d _ e) =
        Binding Nothing c Nothing (fmap denoteBindingAnnotation d) Nothing (denote e)

    denoteBindingAnnotation (_, f) = (Nothing, denote f)

    denoteFunctionBinding (FunctionBinding _ l _ _ t) =
        FunctionBinding Nothing l Nothing Nothing (denote t)

-- | The \"opposite\" of `denote`, like @first absurd@ but faster
renote :: Expr Void a -> Expr s a
renote = unsafeCoerce
{-# INLINE renote #-}

{-| Remove any outermost `Note` constructors

    This is typically used when you want to get the outermost non-`Note`
    constructor without removing internal `Note` constructors
-}
shallowDenote :: Expr s a -> Expr s a
shallowDenote (Note _ e) = shallowDenote e
shallowDenote         e  = e

-- | The set of reserved keywords according to the @keyword@ rule in the grammar
reservedKeywords :: HashSet Text
reservedKeywords =
    Data.HashSet.fromList
        [ "if"
        , "then"
        , "else"
        , "let"
        , "in"
        , "using"
        , "missing"
        , "as"
        , "Infinity"
        , "NaN"
        , "merge"
        , "Some"
        , "toMap"
        , "assert"
        , "forall"
        , "with"
        ]

-- | The set of reserved identifiers for the Dhall language
-- | Contains also all keywords from "reservedKeywords"
reservedIdentifiers :: HashSet Text
reservedIdentifiers = reservedKeywords <>
    Data.HashSet.fromList
        [ -- Builtins according to the `builtin` rule in the grammar
          "Natural/fold"
        , "Natural/build"
        , "Natural/isZero"
        , "Natural/even"
        , "Natural/odd"
        , "Natural/toInteger"
        , "Natural/show"
        , "Natural/subtract"
        , "Integer"
        , "Integer/clamp"
        , "Integer/negate"
        , "Integer/show"
        , "Integer/toDouble"
        , "Integer/show"
        , "Natural/subtract"
        , "Double/show"
        , "List/build"
        , "List/fold"
        , "List/length"
        , "List/head"
        , "List/last"
        , "List/indexed"
        , "List/reverse"
        , "Text/replace"
        , "Text/show"
        , "Date/show"
        , "Time/show"
        , "TimeZone/show"
        , "Bool"
        , "Bytes"
        , "True"
        , "False"
        , "Optional"
        , "None"
        , "Natural"
        , "Integer"
        , "Double"
        , "Text"
        , "Date"
        , "Time"
        , "TimeZone"
        , "List"
        , "Type"
        , "Kind"
        , "Sort"
        ]

{-| `shift` is used by both normalization and type-checking to avoid variable
    capture by shifting variable indices

    For example, suppose that you were to normalize the following expression:

> λ(a : Type) → λ(x : a) → (λ(y : a) → λ(x : a) → y) x

    If you were to substitute @y@ with @x@ without shifting any variable
    indices, then you would get the following incorrect result:

> λ(a : Type) → λ(x : a) → λ(x : a) → x  -- Incorrect normalized form

    In order to substitute @x@ in place of @y@ we need to `shift` @x@ by @1@ in
    order to avoid being misinterpreted as the @x@ bound by the innermost
    lambda.  If we perform that `shift` then we get the correct result:

> λ(a : Type) → λ(x : a) → λ(x : a) → x@1

    As a more worked example, suppose that you were to normalize the following
    expression:

>     λ(a : Type)
> →   λ(f : a → a → a)
> →   λ(x : a)
> →   λ(x : a)
> →   (λ(x : a) → f x x@1) x@1

    The correct normalized result would be:

>     λ(a : Type)
> →   λ(f : a → a → a)
> →   λ(x : a)
> →   λ(x : a)
> →   f x@1 x

    The above example illustrates how we need to both increase and decrease
    variable indices as part of substitution:

    * We need to increase the index of the outer @x\@1@ to @x\@2@ before we
      substitute it into the body of the innermost lambda expression in order
      to avoid variable capture.  This substitution changes the body of the
      lambda expression to @(f x\@2 x\@1)@

    * We then remove the innermost lambda and therefore decrease the indices of
      both @x@s in @(f x\@2 x\@1)@ to @(f x\@1 x)@ in order to reflect that one
      less @x@ variable is now bound within that scope

    Formally, @(shift d (V x n) e)@ modifies the expression @e@ by adding @d@ to
    the indices of all variables named @x@ whose indices are greater than
    @(n + m)@, where @m@ is the number of bound variables of the same name
    within that scope

    In practice, @d@ is always @1@ or @-1@ because we either:

    * increment variables by @1@ to avoid variable capture during substitution
    * decrement variables by @1@ when deleting lambdas after substitution

    @n@ starts off at @0@ when substitution begins and increments every time we
    descend into a lambda or let expression that binds a variable of the same
    name in order to avoid shifting the bound variables by mistake.
-}
shift :: Int -> Var -> Expr s a -> Expr s a
shift d (V x n) (Var (V x' n')) = Var (V x' n'')
  where
    n'' = if x == x' && n <= n' then n' + d else n'
shift d (V x n) (Lam cs (FunctionBinding src0 x' src1 src2 _A) b) =
    Lam cs (FunctionBinding src0 x' src1 src2 _A') b'
  where
    _A' = shift d (V x n ) _A
    b'  = shift d (V x n') b
      where
        n' = if x == x' then n + 1 else n
shift d (V x n) (Pi cs x' _A _B) = Pi cs x' _A' _B'
  where
    _A' = shift d (V x n ) _A
    _B' = shift d (V x n') _B
      where
        n' = if x == x' then n + 1 else n
shift d (V x n) (Let (Binding src0 f src1 mt src2 r) e) =
    Let (Binding src0 f src1 mt' src2 r') e'
  where
    e' = shift d (V x n') e
      where
        n' = if x == f then n + 1 else n

    mt' = fmap (fmap (shift d (V x n))) mt
    r'  =             shift d (V x n)  r
shift d v expression = Lens.over subExpressions (shift d v) expression

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

{-| Utility function used to throw internal errors that should never happen
    (in theory) but that are not enforced by the type system
-}
internalError :: Data.Text.Text -> forall b . b
internalError text = error (unlines
    [ _ERROR <> ": Compiler bug                                                        "
    , "                                                                                "
    , "Explanation: This error message means that there is a bug in the Dhall compiler."
    , "You didn't do anything wrong, but if you would like to see this problem fixed   "
    , "then you should report the bug at:                                              "
    , "                                                                                "
    , "https://github.com/dhall-lang/dhall-haskell/issues                              "
    , "                                                                                "
    , "Please include the following text in your bug report:                           "
    , "                                                                                "
    , "```                                                                             "
    , Data.Text.unpack text <> "                                                       "
    , "```                                                                             "
    ] )
