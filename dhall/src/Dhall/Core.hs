{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core (
    -- * Syntax
      Const(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , DhallDouble(..)
    , Var(..)
    , Binding(..)
    , makeBinding
    , Chunks(..)
    , Expr(..)

    -- * Normalization
    , alphaNormalize
    , normalize
    , normalizeWith
    , normalizeWithM
    , Normalizer
    , NormalizerM
    , ReifiedNormalizer (..)
    , judgmentallyEqual
    , subst
    , shift
    , isNormalized
    , isNormalizedWith
    , denote
    , renote
    , shallowDenote
    , freeIn

    -- * Pretty-printing
    , pretty

    -- * Optics
    , subExpressions
    , chunkExprs
    , bindingExprs

    -- * Let-blocks
    , multiLet
    , wrapInLets
    , MultiLet(..)

    -- * Miscellaneous
    , internalError
    , reservedIdentifiers
    , escapeText
    , pathCharacter
    , throws
    , Eval.textShow
    , censorExpression
    , censorText
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable
import Data.Functor.Identity (Identity(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequence (ViewL(..), ViewR(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Traversable
import Dhall.Src (Src(..))
import Dhall.Syntax
import Dhall.Pretty.Internal
import Instances.TH.Lift ()
import Lens.Family (over)
import Prelude hiding (succ)

import qualified Control.Exception
import qualified Dhall.Eval    as Eval
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Dhall.Map
import qualified Dhall.Set

{-| Returns `True` if two expressions are α-equivalent and β-equivalent and
    `False` otherwise

    `judgmentallyEqual` can fail with an `error` if you compare ill-typed
    expressions
-}
judgmentallyEqual :: Eq a => Expr s a -> Expr t a -> Bool
judgmentallyEqual = Eval.judgmentallyEqual
{-# INLINE judgmentallyEqual #-}

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
shift _ _ (Const a) = Const a
shift d (V x n) (Var (V x' n')) = Var (V x' n'')
  where
    n'' = if x == x' && n <= n' then n' + d else n'
shift d (V x n) (Lam x' _A b) = Lam x' _A' b'
  where
    _A' = shift d (V x n ) _A
    b'  = shift d (V x n') b
      where
        n' = if x == x' then n + 1 else n
shift d (V x n) (Pi x' _A _B) = Pi x' _A' _B'
  where
    _A' = shift d (V x n ) _A
    _B' = shift d (V x n') _B
      where
        n' = if x == x' then n + 1 else n
shift d v (App f a) = App f' a'
  where
    f' = shift d v f
    a' = shift d v a
shift d (V x n) (Let (Binding src0 f src1 mt src2 r) e) =
    Let (Binding src0 f src1 mt' src2 r') e'
  where
    e' = shift d (V x n') e
      where
        n' = if x == f then n + 1 else n

    mt' = fmap (fmap (shift d (V x n))) mt
    r'  =             shift d (V x n)  r
shift d v (Annot a b) = Annot a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ Bool = Bool
shift _ _ (BoolLit a) = BoolLit a
shift d v (BoolAnd a b) = BoolAnd a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolOr a b) = BoolOr a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolEQ a b) = BoolEQ a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolNE a b) = BoolNE a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolIf a b c) = BoolIf a' b' c'
  where
    a' = shift d v a
    b' = shift d v b
    c' = shift d v c
shift _ _ Natural = Natural
shift _ _ (NaturalLit a) = NaturalLit a
shift _ _ NaturalFold = NaturalFold
shift _ _ NaturalBuild = NaturalBuild
shift _ _ NaturalIsZero = NaturalIsZero
shift _ _ NaturalEven = NaturalEven
shift _ _ NaturalOdd = NaturalOdd
shift _ _ NaturalToInteger = NaturalToInteger
shift _ _ NaturalShow = NaturalShow
shift _ _ NaturalSubtract = NaturalSubtract
shift d v (NaturalPlus a b) = NaturalPlus a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (NaturalTimes a b) = NaturalTimes a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ Integer = Integer
shift _ _ (IntegerLit a) = IntegerLit a
shift _ _ IntegerShow = IntegerShow
shift _ _ IntegerToDouble = IntegerToDouble
shift _ _ Double = Double
shift _ _ (DoubleLit a) = DoubleLit a
shift _ _ DoubleShow = DoubleShow
shift _ _ Text = Text
shift d v (TextLit (Chunks a b)) = TextLit (Chunks a' b)
  where
    a' = fmap (fmap (shift d v)) a
shift d v (TextAppend a b) = TextAppend a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ TextShow = TextShow
shift _ _ List = List
shift d v (ListLit a b) = ListLit a' b'
  where
    a' = fmap (shift d v) a
    b' = fmap (shift d v) b
shift _ _ ListBuild = ListBuild
shift d v (ListAppend a b) = ListAppend a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ ListFold = ListFold
shift _ _ ListLength = ListLength
shift _ _ ListHead = ListHead
shift _ _ ListLast = ListLast
shift _ _ ListIndexed = ListIndexed
shift _ _ ListReverse = ListReverse
shift _ _ Optional = Optional
shift d v (Some a) = Some a'
  where
    a' = shift d v a
shift _ _ None = None
shift _ _ OptionalFold = OptionalFold
shift _ _ OptionalBuild = OptionalBuild
shift d v (Record a) = Record a'
  where
    a' = fmap (shift d v) a
shift d v (RecordLit a) = RecordLit a'
  where
    a' = fmap (shift d v) a
shift d v (Union a) = Union a'
  where
    a' = fmap (fmap (shift d v)) a
shift d v (Combine a b) = Combine a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (CombineTypes a b) = CombineTypes a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (Prefer a b) = Prefer a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (RecordCompletion a b) = RecordCompletion a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (Merge a b c) = Merge a' b' c'
  where
    a' =       shift d v  a
    b' =       shift d v  b
    c' = fmap (shift d v) c
shift d v (ToMap a b) = ToMap a' b'
  where
    a' =       shift d v  a
    b' = fmap (shift d v) b
shift d v (Field a b) = Field a' b
  where
    a' = shift d v a
shift d v (Assert a) = Assert a'
  where
    a' = shift d v a
shift d v (Equivalent a b) = Equivalent a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (Project a b) = Project a' b'
  where
    a' =       shift d v  a
    b' = fmap (shift d v) b
shift d v (Note a b) = Note a b'
  where
    b' = shift d v b
shift d v (ImportAlt a b) = ImportAlt a' b'
  where
    a' = shift d v a
    b' = shift d v b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `shift` does nothing to a closed expression
shift _ _ (Embed p) = Embed p

{-| Substitute all occurrences of a variable with an expression

> subst x C B  ~  B[x := C]
-}
subst :: Var -> Expr s a -> Expr s a -> Expr s a
subst _ _ (Const a) = Const a
subst (V x n) e (Lam y _A b) = Lam y _A' b'
  where
    _A' = subst (V x n )                  e  _A
    b'  = subst (V x n') (shift 1 (V y 0) e)  b
    n'  = if x == y then n + 1 else n
subst (V x n) e (Pi y _A _B) = Pi y _A' _B'
  where
    _A' = subst (V x n )                  e  _A
    _B' = subst (V x n') (shift 1 (V y 0) e) _B
    n'  = if x == y then n + 1 else n
subst v e (App f a) = App f' a'
  where
    f' = subst v e f
    a' = subst v e a
subst v e (Var v') = if v == v' then e else Var v'
subst (V x n) e (Let (Binding src0 f src1 mt src2 r) b) =
    Let (Binding src0 f src1 mt' src2 r') b'
  where
    b' = subst (V x n') (shift 1 (V f 0) e) b
      where
        n' = if x == f then n + 1 else n

    mt' = fmap (fmap (subst (V x n) e)) mt
    r'  =             subst (V x n) e  r
subst x e (Annot a b) = Annot a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ Bool = Bool
subst _ _ (BoolLit a) = BoolLit a
subst x e (BoolAnd a b) = BoolAnd a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolOr a b) = BoolOr a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolEQ a b) = BoolEQ a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolNE a b) = BoolNE a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolIf a b c) = BoolIf a' b' c'
  where
    a' = subst x e a
    b' = subst x e b
    c' = subst x e c
subst _ _ Natural = Natural
subst _ _ (NaturalLit a) = NaturalLit a
subst _ _ NaturalFold = NaturalFold
subst _ _ NaturalBuild = NaturalBuild
subst _ _ NaturalIsZero = NaturalIsZero
subst _ _ NaturalEven = NaturalEven
subst _ _ NaturalOdd = NaturalOdd
subst _ _ NaturalToInteger = NaturalToInteger
subst _ _ NaturalShow = NaturalShow
subst _ _ NaturalSubtract = NaturalSubtract
subst x e (NaturalPlus a b) = NaturalPlus a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (NaturalTimes a b) = NaturalTimes a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ Integer = Integer
subst _ _ (IntegerLit a) = IntegerLit a
subst _ _ IntegerShow = IntegerShow
subst _ _ IntegerToDouble = IntegerToDouble
subst _ _ Double = Double
subst _ _ (DoubleLit a) = DoubleLit a
subst _ _ DoubleShow = DoubleShow
subst _ _ Text = Text
subst x e (TextLit (Chunks a b)) = TextLit (Chunks a' b)
  where
    a' = fmap (fmap (subst x e)) a
subst x e (TextAppend a b) = TextAppend a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ TextShow = TextShow
subst _ _ List = List
subst x e (ListLit a b) = ListLit a' b'
  where
    a' = fmap (subst x e) a
    b' = fmap (subst x e) b
subst x e (ListAppend a b) = ListAppend a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ ListBuild = ListBuild
subst _ _ ListFold = ListFold
subst _ _ ListLength = ListLength
subst _ _ ListHead = ListHead
subst _ _ ListLast = ListLast
subst _ _ ListIndexed = ListIndexed
subst _ _ ListReverse = ListReverse
subst _ _ Optional = Optional
subst x e (Some a) = Some a'
  where
    a' = subst x e a
subst _ _ None = None
subst _ _ OptionalFold = OptionalFold
subst _ _ OptionalBuild = OptionalBuild
subst x e (Record kts) = Record kts'
  where
    kts' = fmap (subst x e) kts
subst x e (RecordLit kvs) = RecordLit kvs'
  where
    kvs' = fmap (subst x e) kvs
subst x e (Union kts) = Union kts'
  where
    kts' = fmap (fmap (subst x e)) kts
subst x e (Combine a b) = Combine a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (CombineTypes a b) = CombineTypes a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (Prefer a b) = Prefer a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (RecordCompletion a b) = RecordCompletion a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (Merge a b c) = Merge a' b' c'
  where
    a' =       subst x e  a
    b' =       subst x e  b
    c' = fmap (subst x e) c
subst x e (ToMap a b) = ToMap a' b'
  where
    a' =       subst x e  a
    b' = fmap (subst x e) b
subst x e (Field a b) = Field a' b
  where
    a' = subst x e a
subst x e (Project a b) = Project a' b'
  where
    a' =       subst x e  a
    b' = fmap (subst x e) b
subst x e (Assert a) = Assert a'
  where
    a' = subst x e a
subst x e (Equivalent a b) = Equivalent a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (Note a b) = Note a b'
  where
    b' = subst x e b
subst x e (ImportAlt a b) = ImportAlt a' b'
  where
    a' = subst x e a
    b' = subst x e b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `subst` does nothing to a closed expression
subst _ _ (Embed p) = Embed p

{-| This function is used to determine whether folds like @Natural/fold@ or
    @List/fold@ should be lazy or strict in their accumulator based on the type
    of the accumulator

    If this function returns `True`, then they will be strict in their
    accumulator since we can guarantee an upper bound on the amount of work to
    normalize the accumulator on each step of the loop.  If this function
    returns `False` then they will be lazy in their accumulator and only
    normalize the final result at the end of the fold
-}
boundedType :: Expr s a -> Bool
boundedType Bool             = True
boundedType Natural          = True
boundedType Integer          = True
boundedType Double           = True
boundedType Text             = True
boundedType (App List _)     = False
boundedType (App Optional t) = boundedType t
boundedType (Record kvs)     = all boundedType kvs
boundedType (Union kvs)      = all (all boundedType) kvs
boundedType _                = False

{-| α-normalize an expression by renaming all bound variables to @\"_\"@ and
    using De Bruijn indices to distinguish them

>>> alphaNormalize (Lam "a" (Const Type) (Lam "b" (Const Type) (Lam "x" "a" (Lam "y" "b" "x"))))
Lam "_" (Const Type) (Lam "_" (Const Type) (Lam "_" (Var (V "_" 1)) (Lam "_" (Var (V "_" 1)) (Var (V "_" 1)))))

    α-normalization does not affect free variables:

>>> alphaNormalize "x"
Var (V "x" 0)

-}
alphaNormalize :: Expr s a -> Expr s a
alphaNormalize = Eval.alphaNormalize
{-# INLINE alphaNormalize #-}

{-| Reduce an expression to its normal form, performing beta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.

    `normalize` can also fail with `error` if you normalize an ill-typed
    expression
-}
normalize :: Eq a => Expr s a -> Expr t a
normalize = Eval.normalize
{-# INLINE normalize #-}

{-| Reduce an expression to its normal form, performing beta reduction and applying
    any custom definitions.

    `normalizeWith` is designed to be used with function `typeWith`. The `typeWith`
    function allows typing of Dhall functions in a custom typing context whereas
    `normalizeWith` allows evaluating Dhall expressions in a custom context.

    To be more precise `normalizeWith` applies the given normalizer when it finds an
    application term that it cannot reduce by other means.

    Note that the context used in normalization will determine the properties of normalization.
    That is, if the functions in custom context are not total then the Dhall language, evaluated
    with those functions is not total either.

    `normalizeWith` can fail with an `error` if you normalize an ill-typed
    expression
-}
normalizeWith :: Eq a => Maybe (ReifiedNormalizer a) -> Expr s a -> Expr t a
normalizeWith (Just ctx) t = runIdentity (normalizeWithM (getReifiedNormalizer ctx) t)
normalizeWith _          t = Eval.normalize t

{-| This function generalizes `normalizeWith` by allowing the custom normalizer
    to use an arbitrary `Monad`

    `normalizeWithM` can fail with an `error` if you normalize an ill-typed
    expression
-}
normalizeWithM
    :: (Monad m, Eq a) => NormalizerM m a -> Expr s a -> m (Expr t a)
normalizeWithM ctx e0 = loop (denote e0)
 where
 loop e =  case e of
    Const k -> pure (Const k)
    Var v -> pure (Var v)
    Lam x _A b -> Lam x <$> _A' <*> b'
      where
        _A' = loop _A
        b'  = loop b
    Pi x _A _B -> Pi x <$> _A' <*> _B'
      where
        _A' = loop _A
        _B' = loop _B
    App f a -> do
      res <- ctx (App f a)
      case res of
          Just e1 -> loop e1
          Nothing -> do
              f' <- loop f
              a' <- loop a
              case f' of
                Lam x _A b₀ -> do

                    let a₂ = shift 1 (V x 0) a'
                    let b₁ = subst (V x 0) a₂ b₀
                    let b₂ = shift (-1) (V x 0) b₁

                    loop b₂
                _ -> do
                  case App f' a' of
                    -- build/fold fusion for `List`
                    App (App ListBuild _) (App (App ListFold _) e') -> loop e'

                    App NaturalFold (NaturalLit n) -> do
                        let natural = Var (V "natural" 0)
                        let go 0  x = x
                            go n' x = go (n'-1) (App (Var (V "succ" 0)) x)
                        let n' = go n (Var (V "zero" 0))
                        pure
                            (Lam "natural"
                                (Const Type)
                                (Lam "succ"
                                    (Pi "_" natural natural)
                                    (Lam "zero"
                                        natural
                                        n')))

                    -- build/fold fusion for `Natural`
                    App NaturalBuild (App NaturalFold e') -> loop e'

                    -- build/fold fusion for `Optional`
                    App (App OptionalBuild _) (App (App OptionalFold _) e') -> loop e'

                    App (App (App (App NaturalFold (NaturalLit n0)) t) succ') zero -> do
                      t' <- loop t
                      if boundedType t' then strict else lazy
                      where
                        strict =       strictLoop (fromIntegral n0 :: Integer)
                        lazy   = loop (  lazyLoop (fromIntegral n0 :: Integer))

                        strictLoop !0 = loop zero
                        strictLoop !n = App succ' <$> strictLoop (n - 1) >>= loop

                        lazyLoop !0 = zero
                        lazyLoop !n = App succ' (lazyLoop (n - 1))
                    App NaturalBuild g -> loop (App (App (App g Natural) succ) zero)
                      where
                        succ = Lam "n" Natural (NaturalPlus "n" (NaturalLit 1))

                        zero = NaturalLit 0
                    App NaturalIsZero (NaturalLit n) -> pure (BoolLit (n == 0))
                    App NaturalEven (NaturalLit n) -> pure (BoolLit (even n))
                    App NaturalOdd (NaturalLit n) -> pure (BoolLit (odd n))
                    App NaturalToInteger (NaturalLit n) -> pure (IntegerLit (toInteger n))
                    App NaturalShow (NaturalLit n) ->
                        pure (TextLit (Chunks [] (Data.Text.pack (show n))))
                    App (App NaturalSubtract (NaturalLit x)) (NaturalLit y)
                        | y >= x    -> pure (NaturalLit (subtract x y))
                        | otherwise -> pure (NaturalLit 0)
                    App (App NaturalSubtract (NaturalLit 0)) y -> pure y
                    App (App NaturalSubtract _) (NaturalLit 0) -> pure (NaturalLit 0)
                    App (App NaturalSubtract x) y | Eval.judgmentallyEqual x y -> pure (NaturalLit 0)
                    App IntegerShow (IntegerLit n)
                        | 0 <= n    -> pure (TextLit (Chunks [] ("+" <> Data.Text.pack (show n))))
                        | otherwise -> pure (TextLit (Chunks [] (Data.Text.pack (show n))))
                    -- `(read . show)` is used instead of `fromInteger` because `read` uses
                    -- the correct rounding rule.
                    -- See https://gitlab.haskell.org/ghc/ghc/issues/17231.
                    App IntegerToDouble (IntegerLit n) -> pure (DoubleLit ((DhallDouble . read . show) n))
                    App DoubleShow (DoubleLit (DhallDouble n)) ->
                        pure (TextLit (Chunks [] (Data.Text.pack (show n))))
                    App (App OptionalBuild _A₀) g ->
                        loop (App (App (App g optional) just) nothing)
                      where
                        optional = App Optional _A₀

                        just = Lam "a" _A₀ (Some "a")

                        nothing = App None _A₀
                    App (App ListBuild _A₀) g -> loop (App (App (App g list) cons) nil)
                      where
                        _A₁ = shift 1 "a" _A₀

                        list = App List _A₀

                        cons =
                            Lam "a" _A₀
                                (Lam "as"
                                    (App List _A₁)
                                    (ListAppend (ListLit Nothing (pure "a")) "as")
                                )

                        nil = ListLit (Just (App List _A₀)) empty
                    App (App ListFold t) (ListLit _ xs) -> do
                        t' <- loop t
                        let list = Var (V "list" 0)
                        let lam term =
                                Lam "list" (Const Type)
                                    (Lam "cons" (Pi "_" t' (Pi "_" list list))
                                        (Lam "nil" list term))
                        term <- foldrM
                            (\x acc -> do
                                x' <- loop x
                                pure (App (App (Var (V "cons" 0)) x') acc))
                            (Var (V "nil" 0))
                            xs
                        pure (lam term)
                    App (App (App (App (App ListFold _) (ListLit _ xs)) t) cons) nil -> do
                      t' <- loop t
                      if boundedType t' then strict else lazy
                      where
                        strict =       foldr strictCons strictNil xs
                        lazy   = loop (foldr   lazyCons   lazyNil xs)

                        strictNil = loop nil
                        lazyNil   =      nil

                        strictCons y ys = do
                          App (App cons y) <$> ys >>= loop
                        lazyCons   y ys =       App (App cons y) ys
                    App (App ListLength _) (ListLit _ ys) ->
                        pure (NaturalLit (fromIntegral (Data.Sequence.length ys)))
                    App (App ListHead t) (ListLit _ ys) -> loop o
                      where
                        o = case Data.Sequence.viewl ys of
                                y :< _ -> Some y
                                _      -> App None t
                    App (App ListLast t) (ListLit _ ys) -> loop o
                      where
                        o = case Data.Sequence.viewr ys of
                                _ :> y -> Some y
                                _      -> App None t
                    App (App ListIndexed _A₀) (ListLit _ as₀) -> loop (ListLit t as₁)
                      where
                        as₁ = Data.Sequence.mapWithIndex adapt as₀

                        _A₂ = Record (Dhall.Map.fromList kts)
                          where
                            kts = [ ("index", Natural)
                                  , ("value", _A₀)
                                  ]

                        t | null as₀  = Just (App List _A₂)
                          | otherwise = Nothing

                        adapt n a_ =
                            RecordLit (Dhall.Map.fromList kvs)
                          where
                            kvs = [ ("index", NaturalLit (fromIntegral n))
                                  , ("value", a_)
                                  ]
                    App (App ListReverse _) (ListLit t xs) ->
                        loop (ListLit t (Data.Sequence.reverse xs))

                    App (App OptionalFold t0) x0 -> do
                        t1 <- loop t0
                        let optional = Var (V "optional" 0)
                        let lam term = (Lam "optional"
                                           (Const Type)
                                           (Lam "some"
                                               (Pi "_" t1 optional)
                                               (Lam "none" optional term)))
                        x1 <- loop x0
                        pure $ case x1 of
                            App None _ -> lam (Var (V "none" 0))
                            Some x'    -> lam (App (Var (V "some" 0)) x')
                            _          -> App (App OptionalFold t1) x1

                    App TextShow (TextLit (Chunks [] oldText)) ->
                        loop (TextLit (Chunks [] newText))
                      where
                        newText = Eval.textShow oldText
                    _ -> do
                        res2 <- ctx (App f' a')
                        case res2 of
                            Nothing -> pure (App f' a')
                            Just app' -> loop app'
    Let (Binding _ f _ _ _ r) b -> loop b''
      where
        r'  = shift   1  (V f 0) r
        b'  = subst (V f 0) r' b
        b'' = shift (-1) (V f 0) b'
    Annot x _ -> loop x
    Bool -> pure Bool
    BoolLit b -> pure (BoolLit b)
    BoolAnd x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit True )  r              = r
        decide (BoolLit False)  _              = BoolLit False
        decide  l              (BoolLit True ) = l
        decide  _              (BoolLit False) = BoolLit False
        decide  l               r
            | Eval.judgmentallyEqual l r = l
            | otherwise                  = BoolAnd l r
    BoolOr x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit False)  r              = r
        decide (BoolLit True )  _              = BoolLit True
        decide  l              (BoolLit False) = l
        decide  _              (BoolLit True ) = BoolLit True
        decide  l               r
            | Eval.judgmentallyEqual l r = l
            | otherwise                  = BoolOr l r
    BoolEQ x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit True )  r              = r
        decide  l              (BoolLit True ) = l
        decide  l               r
            | Eval.judgmentallyEqual l r = BoolLit True
            | otherwise                  = BoolEQ l r
    BoolNE x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit False)  r              = r
        decide  l              (BoolLit False) = l
        decide  l               r
            | Eval.judgmentallyEqual l r = BoolLit False
            | otherwise                  = BoolNE l r
    BoolIf bool true false -> decide <$> loop bool <*> loop true <*> loop false
      where
        decide (BoolLit True )  l              _              = l
        decide (BoolLit False)  _              r              = r
        decide  b              (BoolLit True) (BoolLit False) = b
        decide  b               l              r
            | Eval.judgmentallyEqual l r = l
            | otherwise                  = BoolIf b l r
    Natural -> pure Natural
    NaturalLit n -> pure (NaturalLit n)
    NaturalFold -> pure NaturalFold
    NaturalBuild -> pure NaturalBuild
    NaturalIsZero -> pure NaturalIsZero
    NaturalEven -> pure NaturalEven
    NaturalOdd -> pure NaturalOdd
    NaturalToInteger -> pure NaturalToInteger
    NaturalShow -> pure NaturalShow
    NaturalSubtract -> pure NaturalSubtract
    NaturalPlus x y -> decide <$> loop x <*> loop y
      where
        decide (NaturalLit 0)  r             = r
        decide  l             (NaturalLit 0) = l
        decide (NaturalLit m) (NaturalLit n) = NaturalLit (m + n)
        decide  l              r             = NaturalPlus l r
    NaturalTimes x y -> decide <$> loop x <*> loop y
      where
        decide (NaturalLit 1)  r             = r
        decide  l             (NaturalLit 1) = l
        decide (NaturalLit 0)  _             = NaturalLit 0
        decide  _             (NaturalLit 0) = NaturalLit 0
        decide (NaturalLit m) (NaturalLit n) = NaturalLit (m * n)
        decide  l              r             = NaturalTimes l r
    Integer -> pure Integer
    IntegerLit n -> pure (IntegerLit n)
    IntegerShow -> pure IntegerShow
    IntegerToDouble -> pure IntegerToDouble
    Double -> pure Double
    DoubleLit n -> pure (DoubleLit n)
    DoubleShow -> pure DoubleShow
    Text -> pure Text
    TextLit (Chunks xys z) -> do
        chunks' <- mconcat <$> chunks
        case chunks' of
            Chunks [("", x)] "" -> pure x
            c                   -> pure (TextLit c)
      where
        chunks =
          ((++ [Chunks [] z]) . concat) <$> traverse process xys

        process (x, y) = do
          y' <- loop y
          case y' of
            TextLit c -> pure [Chunks [] x, c]
            _         -> pure [Chunks [(x, y')] mempty]
    TextAppend x y -> loop (TextLit (Chunks [("", x), ("", y)] ""))
    TextShow -> pure TextShow
    List -> pure List
    ListLit t es
        | Data.Sequence.null es -> ListLit <$> t' <*> pure Data.Sequence.empty
        | otherwise             -> ListLit Nothing <$> es'
      where
        t'  = traverse loop t
        es' = traverse loop es
    ListAppend x y -> decide <$> loop x <*> loop y
      where
        decide (ListLit _ m)  r            | Data.Sequence.null m = r
        decide  l            (ListLit _ n) | Data.Sequence.null n = l
        decide (ListLit t m) (ListLit _ n)                        = ListLit t (m <> n)
        decide  l             r                                   = ListAppend l r
    ListBuild -> pure ListBuild
    ListFold -> pure ListFold
    ListLength -> pure ListLength
    ListHead -> pure ListHead
    ListLast -> pure ListLast
    ListIndexed -> pure ListIndexed
    ListReverse -> pure ListReverse
    Optional -> pure Optional
    Some a -> Some <$> a'
      where
        a' = loop a
    None -> pure None
    OptionalFold -> pure OptionalFold
    OptionalBuild -> pure OptionalBuild
    Record kts -> Record . Dhall.Map.sort <$> kts'
      where
        kts' = traverse loop kts
    RecordLit kvs -> RecordLit . Dhall.Map.sort <$> kvs'
      where
        kvs' = traverse loop kvs
    Union kts -> Union . Dhall.Map.sort <$> kts'
      where
        kts' = traverse (traverse loop) kts
    Combine x y -> decide <$> loop x <*> loop y
      where
        decide (RecordLit m) r | Data.Foldable.null m =
            r
        decide l (RecordLit n) | Data.Foldable.null n =
            l
        decide (RecordLit m) (RecordLit n) =
            RecordLit (Dhall.Map.unionWith decide m n)
        decide l r =
            Combine l r
    CombineTypes x y -> decide <$> loop x <*> loop y
      where
        decide (Record m) r | Data.Foldable.null m =
            r
        decide l (Record n) | Data.Foldable.null n =
            l
        decide (Record m) (Record n) =
            Record (Dhall.Map.unionWith decide m n)
        decide l r =
            CombineTypes l r
    Prefer x y -> decide <$> loop x <*> loop y
      where
        decide (RecordLit m) r | Data.Foldable.null m =
            r
        decide l (RecordLit n) | Data.Foldable.null n =
            l
        decide (RecordLit m) (RecordLit n) =
            RecordLit (Dhall.Map.union n m)
        decide l r | Eval.judgmentallyEqual l r =
            l
        decide l r =
            Prefer l r
    RecordCompletion x y -> do
        loop (Annot (Prefer (Field x "default") y) (Field x "Type"))
    Merge x y t      -> do
        x' <- loop x
        y' <- loop y
        case x' of
            RecordLit kvsX ->
                case y' of
                    Field (Union ktsY) kY ->
                        case Dhall.Map.lookup kY ktsY of
                            Just Nothing ->
                                case Dhall.Map.lookup kY kvsX of
                                    Just vX -> return vX
                                    Nothing -> Merge x' y' <$> t'
                            _ ->
                                Merge x' y' <$> t'
                    App (Field (Union ktsY) kY) vY ->
                        case Dhall.Map.lookup kY ktsY of
                            Just (Just _) ->
                                case Dhall.Map.lookup kY kvsX of
                                    Just vX -> loop (App vX vY)
                                    Nothing -> Merge x' y' <$> t'
                            _ ->
                                Merge x' y' <$> t'
                    _ -> Merge x' y' <$> t'
            _ -> Merge x' y' <$> t'
      where
        t' = traverse loop t
    ToMap x t        -> do
        x' <- loop x
        t' <- traverse loop t
        case x' of
            RecordLit kvsX -> do
                let entry (key, value) =
                        RecordLit
                            (Dhall.Map.fromList
                                [ ("mapKey"  , TextLit (Chunks [] key))
                                , ("mapValue", value                  )
                                ]
                            )

                let keyValues = Data.Sequence.fromList (map entry (Dhall.Map.toList kvsX))

                let listType = case t' of
                        Just _ | null keyValues ->
                            t'
                        _ ->
                            Nothing

                return (ListLit listType keyValues)
            _ -> do
                return (ToMap x' t')
    Field r x        -> do
        let singletonRecordLit v = RecordLit (Dhall.Map.singleton x v)

        r' <- loop r
        case r' of
            RecordLit kvs ->
                case Dhall.Map.lookup x kvs of
                    Just v  -> pure v
                    Nothing -> Field <$> (RecordLit <$> traverse loop kvs) <*> pure x
            Project r_ _ -> loop (Field r_ x)
            Prefer (RecordLit kvs) r_ -> case Dhall.Map.lookup x kvs of
                Just v -> pure (Field (Prefer (singletonRecordLit v) r_) x)
                Nothing -> loop (Field r_ x)
            Prefer l (RecordLit kvs) -> case Dhall.Map.lookup x kvs of
                Just v -> pure v
                Nothing -> loop (Field l x)
            Combine (RecordLit kvs) r_ -> case Dhall.Map.lookup x kvs of
                Just v -> pure (Field (Combine (singletonRecordLit v) r_) x)
                Nothing -> loop (Field r_ x)
            Combine l (RecordLit kvs) -> case Dhall.Map.lookup x kvs of
                Just v -> pure (Field (Combine l (singletonRecordLit v)) x)
                Nothing -> loop (Field l x)
            _ -> pure (Field r' x)
    Project x (Left fields)-> do
        x' <- loop x
        let fieldsSet = Dhall.Set.toSet fields
        case x' of
            RecordLit kvs ->
                pure (RecordLit (Dhall.Map.restrictKeys kvs fieldsSet))
            Project y _ ->
                loop (Project y (Left fields))
            Prefer l (RecordLit rKvs) -> do
                let rKs = Dhall.Map.keysSet rKvs
                let l' = Project l (Left (Dhall.Set.fromSet (Data.Set.difference fieldsSet rKs)))
                let r' = RecordLit (Dhall.Map.restrictKeys rKvs fieldsSet)
                loop (Prefer l' r')
            _ | null fields -> pure (RecordLit mempty)
              | otherwise   -> pure (Project x' (Left (Dhall.Set.sort fields)))
    Project r (Right e1) -> do
        e2 <- loop e1

        case e2 of
            Record kts -> do
                loop (Project r (Left (Dhall.Set.fromSet (Dhall.Map.keysSet kts))))
            _ -> do
                r' <- loop r
                pure (Project r' (Right e2))
    Assert t -> do
        t' <- loop t

        pure (Assert t')
    Equivalent l r -> do
        l' <- loop l
        r' <- loop r

        pure (Equivalent l' r')
    Note _ e' -> loop e'
    ImportAlt l _r -> loop l
    Embed a -> pure (Embed a)

-- | Use this to wrap you embedded functions (see `normalizeWith`) to make them
--   polymorphic enough to be used.
type NormalizerM m a = forall s. Expr s a -> m (Maybe (Expr s a))

-- | An variation on `NormalizerM` for pure normalizers
type Normalizer a = NormalizerM Identity a

-- | A reified 'Normalizer', which can be stored in structures without
-- running into impredicative polymorphism.
newtype ReifiedNormalizer a = ReifiedNormalizer
  { getReifiedNormalizer :: Normalizer a }

-- | Check if an expression is in a normal form given a context of evaluation.
--   Unlike `isNormalized`, this will fully normalize and traverse through the expression.
--
--   It is much more efficient to use `isNormalized`.
--
--  `isNormalizedWith` can fail with an `error` if you check an ill-typed
--  expression
isNormalizedWith :: (Eq s, Eq a) => Normalizer a -> Expr s a -> Bool
isNormalizedWith ctx e = e == normalizeWith (Just (ReifiedNormalizer ctx)) e

-- | Quickly check if an expression is in normal form
--
-- Given a well-typed expression @e@, @'isNormalized' e@ is equivalent to
-- @e == 'normalize' e@.
--
-- Given an ill-typed expression, 'isNormalized' may fail with an error, or
-- evaluate to either False or True!
isNormalized :: Eq a => Expr s a -> Bool
isNormalized e0 = loop (denote e0)
  where
    loop e = case e of
      Const _ -> True
      Var _ -> True
      Lam _ a b -> loop a && loop b
      Pi _ a b -> loop a && loop b
      App f a -> loop f && loop a && case App f a of
          App (Lam _ _ _) _ -> False

          -- build/fold fusion for `List`
          App (App ListBuild _) (App (App ListFold _) _) -> False

          -- build/fold fusion for `Natural`
          App NaturalBuild (App NaturalFold _) -> False

          -- build/fold fusion for `Optional`
          App (App OptionalBuild _) (App (App OptionalFold _) _) -> False

          App (App (App (App NaturalFold (NaturalLit _)) _) _) _ -> False
          App NaturalFold (NaturalLit _) -> False
          App NaturalBuild _ -> False
          App NaturalIsZero (NaturalLit _) -> False
          App NaturalEven (NaturalLit _) -> False
          App NaturalOdd (NaturalLit _) -> False
          App NaturalShow (NaturalLit _) -> False
          App (App NaturalSubtract (NaturalLit _)) (NaturalLit _) -> False
          App (App NaturalSubtract (NaturalLit 0)) _ -> False
          App (App NaturalSubtract _) (NaturalLit 0) -> False
          App (App NaturalSubtract x) y -> not (Eval.judgmentallyEqual x y)
          App NaturalToInteger (NaturalLit _) -> False
          App IntegerShow (IntegerLit _) -> False
          App IntegerToDouble (IntegerLit _) -> False
          App DoubleShow (DoubleLit _) -> False
          App (App OptionalBuild _) _ -> False
          App (App ListBuild _) _ -> False
          App (App ListFold _) (ListLit _ _) -> False
          App (App ListLength _) (ListLit _ _) -> False
          App (App ListHead _) (ListLit _ _) -> False
          App (App ListLast _) (ListLit _ _) -> False
          App (App ListIndexed _) (ListLit _ _) -> False
          App (App ListReverse _) (ListLit _ _) -> False
          App (App OptionalFold _) (Some _) -> False
          App (App OptionalFold _) (App None _) -> False
          App TextShow (TextLit (Chunks [] _)) ->
              False
          _ -> True
      Let _ _ -> False
      Annot _ _ -> False
      Bool -> True
      BoolLit _ -> True
      BoolAnd x y -> loop x && loop y && decide x y
        where
          decide (BoolLit _)  _          = False
          decide  _          (BoolLit _) = False
          decide  l           r          = not (Eval.judgmentallyEqual l r)
      BoolOr x y -> loop x && loop y && decide x y
        where
          decide (BoolLit _)  _          = False
          decide  _          (BoolLit _) = False
          decide  l           r          = not (Eval.judgmentallyEqual l r)
      BoolEQ x y -> loop x && loop y && decide x y
        where
          decide (BoolLit True)  _             = False
          decide  _             (BoolLit True) = False
          decide  l              r             = not (Eval.judgmentallyEqual l r)
      BoolNE x y -> loop x && loop y && decide x y
        where
          decide (BoolLit False)  _               = False
          decide  _              (BoolLit False ) = False
          decide  l               r               = not (Eval.judgmentallyEqual l r)
      BoolIf x y z ->
          loop x && loop y && loop z && decide x y z
        where
          decide (BoolLit _)  _              _              = False
          decide  _          (BoolLit True) (BoolLit False) = False
          decide  _           l              r              = not (Eval.judgmentallyEqual l r)
      Natural -> True
      NaturalLit _ -> True
      NaturalFold -> True
      NaturalBuild -> True
      NaturalIsZero -> True
      NaturalEven -> True
      NaturalOdd -> True
      NaturalShow -> True
      NaturalSubtract -> True
      NaturalToInteger -> True
      NaturalPlus x y -> loop x && loop y && decide x y
        where
          decide (NaturalLit 0)  _             = False
          decide  _             (NaturalLit 0) = False
          decide (NaturalLit _) (NaturalLit _) = False
          decide  _              _             = True
      NaturalTimes x y -> loop x && loop y && decide x y
        where
          decide (NaturalLit 0)  _             = False
          decide  _             (NaturalLit 0) = False
          decide (NaturalLit 1)  _             = False
          decide  _             (NaturalLit 1) = False
          decide (NaturalLit _) (NaturalLit _) = False
          decide  _              _             = True
      Integer -> True
      IntegerLit _ -> True
      IntegerShow -> True
      IntegerToDouble -> True
      Double -> True
      DoubleLit _ -> True
      DoubleShow -> True
      Text -> True
      TextLit (Chunks [("", _)] "") -> False
      TextLit (Chunks xys _) -> all (all check) xys
        where
          check y = loop y && case y of
              TextLit _ -> False
              _         -> True
      TextAppend _ _ -> False
      TextShow -> True
      List -> True
      ListLit t es -> all loop t && all loop es
      ListAppend x y -> loop x && loop y && decide x y
        where
          decide (ListLit _ m)  _            | Data.Sequence.null m = False
          decide  _            (ListLit _ n) | Data.Sequence.null n = False
          decide (ListLit _ _) (ListLit _ _)                        = False
          decide  _             _                                   = True
      ListBuild -> True
      ListFold -> True
      ListLength -> True
      ListHead -> True
      ListLast -> True
      ListIndexed -> True
      ListReverse -> True
      Optional -> True
      Some a -> loop a
      None -> True
      OptionalFold -> True
      OptionalBuild -> True
      Record kts -> Dhall.Map.isSorted kts && all loop kts
      RecordLit kvs -> Dhall.Map.isSorted kvs && all loop kvs
      Union kts -> Dhall.Map.isSorted kts && all (all loop) kts
      Combine x y -> loop x && loop y && decide x y
        where
          decide (RecordLit m) _ | Data.Foldable.null m = False
          decide _ (RecordLit n) | Data.Foldable.null n = False
          decide (RecordLit _) (RecordLit _) = False
          decide  _ _ = True
      CombineTypes x y -> loop x && loop y && decide x y
        where
          decide (Record m) _ | Data.Foldable.null m = False
          decide _ (Record n) | Data.Foldable.null n = False
          decide (Record _) (Record _) = False
          decide  _ _ = True
      Prefer x y -> loop x && loop y && decide x y
        where
          decide (RecordLit m) _ | Data.Foldable.null m = False
          decide _ (RecordLit n) | Data.Foldable.null n = False
          decide (RecordLit _) (RecordLit _) = False
          decide l r = not (Eval.judgmentallyEqual l r)
      RecordCompletion _ _ -> False
      Merge x y t -> loop x && loop y && all loop t
      ToMap x t -> case x of
          RecordLit _ -> False
          _ -> loop x && all loop t
      Field r k -> case r of
          RecordLit _ -> False
          Project _ _ -> False
          Prefer (RecordLit m) _ -> Dhall.Map.keys m == [k] && loop r
          Prefer _ (RecordLit _) -> False
          Combine (RecordLit m) _ -> Dhall.Map.keys m == [k] && loop r
          Combine _ (RecordLit m) -> Dhall.Map.keys m == [k] && loop r
          _ -> loop r
      Project r p -> loop r &&
          case p of
              Left s -> case r of
                  RecordLit _ -> False
                  Project _ _ -> False
                  Prefer _ (RecordLit _) -> False
                  _ -> not (Dhall.Set.null s) && Dhall.Set.isSorted s
              Right e' -> case e' of
                  Record _ -> False
                  _ -> loop e'
      Assert t -> loop t
      Equivalent l r -> loop l && loop r
      Note _ e' -> loop e'
      ImportAlt _ _ -> False
      Embed _ -> True

{-| Detect if the given variable is free within the given expression

>>> "x" `freeIn` "x"
True
>>> "x" `freeIn` "y"
False
>>> "x" `freeIn` Lam "x" (Const Type) "x"
False
-}
freeIn :: Eq a => Var -> Expr s a -> Bool
variable@(V var i) `freeIn` expression =
    Dhall.Core.subst variable (Var (V var (i + 1))) strippedExpression
      /= strippedExpression
  where
    denote' :: Expr t b -> Expr () b
    denote' = denote

    strippedExpression = denote' expression

-- | Pretty-print a value
pretty :: Pretty a => a -> Text
pretty = pretty_
{-# INLINE pretty #-}

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

{-| Escape a `Text` literal using Dhall's escaping rules

    Note that the result does not include surrounding quotes
-}
escapeText :: Text -> Text
escapeText = escapeText_
{-# INLINE escapeText #-}


{-| Utility used to implement the @--censor@ flag, by:

    * Replacing all `Src` text with spaces
    * Replacing all `Text` literals inside type errors with spaces
-}
censorExpression :: Expr Src a -> Expr Src a
censorExpression (TextLit chunks) = TextLit (censorChunks chunks)
censorExpression (Note src     e) = Note (censorSrc src) (censorExpression e)
censorExpression  e               = over subExpressions censorExpression e

censorChunks :: Chunks Src a -> Chunks Src a
censorChunks (Chunks xys z) = Chunks xys' z'
  where
    z' = censorText z

    xys' = [ (censorText x, censorExpression y) | (x, y) <- xys ]

-- | Utility used to censor `Text` by replacing all characters with a space
censorText :: Text -> Text
censorText = Data.Text.map (\_ -> ' ')

censorSrc :: Src -> Src
censorSrc (Src { srcText = oldText, .. }) = Src { srcText = newText, .. }
  where
    newText = censorText oldText

{-| Convenience utility for converting `Either`-based exceptions to `IO`-based
    exceptions
-}
throws :: (Exception e, MonadIO io) => Either e a -> io a
throws (Left  e) = liftIO (Control.Exception.throwIO e)
throws (Right r) = return r

{- $setup
>>> import qualified Codec.Serialise
>>> import qualified Dhall.Binary
>>> import Data.SpecialValues
>>> import Test.QuickCheck (Arbitrary(..), oneof, elements)
>>> :{
  instance Arbitrary DhallDouble where
    arbitrary = fmap DhallDouble (oneof [ arbitrary, elements specialValues ])
:}
-}
