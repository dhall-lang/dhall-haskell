{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Dhall.Normalize (
      alphaNormalize
    , normalize
    , normalizeWith
    , normalizeWithM
    , Normalizer
    , NormalizerM
    , ReifiedNormalizer (..)
    , judgmentallyEqual
    , subst
    , Syntax.shift
    , isNormalized
    , isNormalizedWith
    , freeIn
    ) where

import Control.Applicative   (empty)
import Data.Foldable
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Sequence         (ViewL (..), ViewR (..))
import Data.Traversable
import Instances.TH.Lift     ()
import Prelude               hiding (succ)

import Dhall.Syntax
    ( Binding (Binding)
    , Chunks (..)
    , DhallDouble (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , PreferAnnotation (..)
    , RecordField (..)
    , Var (..)
    , WithComponent (..)
    )

import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text     as Text
import qualified Dhall.Eval    as Eval
import qualified Dhall.Map
import qualified Dhall.Syntax  as Syntax
import qualified Lens.Micro    as Lens

{-| Returns `True` if two expressions are α-equivalent and β-equivalent and
    `False` otherwise

    `judgmentallyEqual` can fail with an `error` if you compare ill-typed
    expressions
-}
judgmentallyEqual :: Eq a => Expr s a -> Expr t a -> Bool
judgmentallyEqual = Eval.judgmentallyEqual
{-# INLINE judgmentallyEqual #-}

{-| Substitute all occurrences of a variable with an expression

> subst x C B  ~  B[x := C]
-}
subst :: Var -> Expr s a -> Expr s a -> Expr s a
subst _ _ (Const a) = Const a
subst (V x n) e (Lam cs (FunctionBinding src0 y src1 src2 _A) b) =
    Lam cs (FunctionBinding src0 y src1 src2 _A') b'
  where
    _A' = subst (V x n )                         e  _A
    b'  = subst (V x n') (Syntax.shift 1 (V y 0) e)  b
    n'  = if x == y then n + 1 else n
subst (V x n) e (Pi cs y _A _B) = Pi cs y _A' _B'
  where
    _A' = subst (V x n )                         e  _A
    _B' = subst (V x n') (Syntax.shift 1 (V y 0) e) _B
    n'  = if x == y then n + 1 else n
subst v e (Var v') = if v == v' then e else Var v'
subst (V x n) e (Let (Binding src0 f src1 mt src2 r) b) =
    Let (Binding src0 f src1 mt' src2 r') b'
  where
    b' = subst (V x n') (Syntax.shift 1 (V f 0) e) b
      where
        n' = if x == f then n + 1 else n

    mt' = fmap (fmap (subst (V x n) e)) mt
    r'  =             subst (V x n) e  r
subst x e expression = Lens.over Syntax.subExpressions (subst x e) expression

{-| This function is used to determine whether folds like @Natural/fold@ or
    @List/fold@ should be lazy or strict in their accumulator based on the type
    of the accumulator

    If this function returns `True`, then they will be strict in their
    accumulator since we can guarantee an upper bound on the amount of work to
    normalize the accumulator on each step of the loop.  If this function
    returns `False` then they will be lazy in their accumulator and only
    normalize the final result at the end of the fold

    For @Natural/fold@ on "bounded types", the short-circuit optimization will be applied.
    See @Dhall.Eval.boundedType@ for comparison.
-}
boundedType :: Expr s a -> Bool
boundedType Bool             = True
boundedType Natural          = True
boundedType Integer          = True
boundedType Double           = True
boundedType Text             = True
boundedType (App List _)     = False
boundedType (App Optional t) = boundedType t
boundedType (Record kvs)     = all (boundedType . recordFieldValue) kvs
boundedType (Union kvs)      = all (all boundedType) kvs
boundedType _                = False

{-| α-normalize an expression by renaming all bound variables to @\"_\"@ and
    using De Bruijn indices to distinguish them

>>> mfb = Syntax.makeFunctionBinding
>>> alphaNormalize (Lam mempty (mfb "a" (Const Type)) (Lam mempty (mfb "b" (Const Type)) (Lam mempty (mfb "x" "a") (Lam mempty (mfb "y" "b") "x"))))
Lam AutoInferCharSet (FunctionBinding {functionBindingSrc0 = Nothing, functionBindingVariable = "_", functionBindingSrc1 = Nothing, functionBindingSrc2 = Nothing, functionBindingAnnotation = Const Type}) (Lam AutoInferCharSet (FunctionBinding {functionBindingSrc0 = Nothing, functionBindingVariable = "_", functionBindingSrc1 = Nothing, functionBindingSrc2 = Nothing, functionBindingAnnotation = Const Type}) (Lam AutoInferCharSet (FunctionBinding {functionBindingSrc0 = Nothing, functionBindingVariable = "_", functionBindingSrc1 = Nothing, functionBindingSrc2 = Nothing, functionBindingAnnotation = Var (V "_" 1)}) (Lam AutoInferCharSet (FunctionBinding {functionBindingSrc0 = Nothing, functionBindingVariable = "_", functionBindingSrc1 = Nothing, functionBindingSrc2 = Nothing, functionBindingAnnotation = Var (V "_" 1)}) (Var (V "_" 1)))))

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

    `normalizeWith` is designed to be used with function `Dhall.TypeCheck.typeWith`. The `Dhall.TypeCheck.typeWith`
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
{-# INLINABLE normalizeWith #-}

{-| This function generalizes `normalizeWith` by allowing the custom normalizer
    to use an arbitrary `Monad`

    `normalizeWithM` can fail with an `error` if you normalize an ill-typed
    expression
-}
normalizeWithM
    :: (Monad m, Eq a) => NormalizerM m a -> Expr s a -> m (Expr t a)
normalizeWithM ctx e0 = fmap (Syntax.renote . Syntax.denote) (loop (Syntax.denote e0))
 where
  loop e = ctx e >>= \case
      Just e' -> loop e'
      Nothing -> case e of
          Const k -> pure (Const k)
          Var v -> pure (Var v)
          Lam cs (FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = _A }) b ->
              Lam cs <$> (Syntax.makeFunctionBinding x <$> _A') <*> b'
            where
              _A' = loop _A
              b'  = loop b
          Pi cs x _A _B -> Pi cs x <$> _A' <*> _B'
            where
              _A' = loop _A
              _B' = loop _B
          App f a -> do
              f' <- loop f
              a' <- loop a
              case f' of
                Lam _ (FunctionBinding _ x _ _ _A) b₀ -> do

                    let a₂ = Syntax.shift 1 (V x 0) a'
                    let b₁ = subst (V x 0) a₂ b₀
                    let b₂ = Syntax.shift (-1) (V x 0) b₁

                    loop b₂
                _ ->
                  case App f' a' of
                    App (App (App (App (Var (V "Natural/fold" 0)) (NaturalLit n0)) t) succ') zero -> do
                      t' <- loop t
                      -- `boundedType` is checked once, not repeated in the loop.
                      if boundedType t' then strict else lazy
                      where
                        -- Use an `Integer` for the loop, due to the following
                        -- issue:
                        --
                        -- https://github.com/ghcjs/ghcjs/issues/782
                        strict =       strictLoop (fromIntegral n0 :: Integer)
                        lazy   = loop (  lazyLoop (fromIntegral n0 :: Integer))

                        strictLoop !n = do
                            z <- loop zero
                            strictLoopShortcut n z

                        strictLoopShortcut 0 !previous = pure previous
                        strictLoopShortcut !n !previous = do
                            current <- loop (App succ' previous)
                            if judgmentallyEqual previous current
                                then pure previous
                                else strictLoopShortcut (n - 1) current

                        lazyLoop 0 = zero
                        lazyLoop !n = App succ' (lazyLoop (n - 1))
                    App (Var (V "Natural/build" 0)) g -> loop (App (App (App g Natural) succ) zero)
                      where
                        succ = Lam mempty (Syntax.makeFunctionBinding "n" Natural) (NaturalPlus "n" (NaturalLit 1))

                        zero = NaturalLit 0
                    App (Var (V "Natural/isZero" 0)) (NaturalLit n) -> pure (BoolLit (n == 0))
                    App (Var (V "Natural/even" 0)) (NaturalLit n) -> pure (BoolLit (even n))
                    App (Var (V "Natural/odd" 0)) (NaturalLit n) -> pure (BoolLit (odd n))
                    App (Var (V "Natural/toInteger" 0)) (NaturalLit n) -> pure (IntegerLit (toInteger n))
                    App (Var (V "Natural/show" 0)) (NaturalLit n) ->
                        pure (TextLit (Chunks [] (Text.pack (show n))))
                    App (App (Var (V "Natural/subtract" 0)) (NaturalLit x)) (NaturalLit y)
                        -- Use an `Integer` for the subtraction, due to the
                        -- following issue:
                        --
                        -- https://github.com/ghcjs/ghcjs/issues/782
                        | y >= x ->
                            pure (NaturalLit (fromIntegral (subtract (fromIntegral x :: Integer) (fromIntegral y :: Integer))))
                        | otherwise ->
                            pure (NaturalLit 0)
                    App (App (Var (V "Natural/subtract" 0)) (NaturalLit 0)) y -> pure y
                    App (App (Var (V "Natural/subtract" 0)) _) (NaturalLit 0) -> pure (NaturalLit 0)
                    App (App (Var (V "Natural/subtract" 0)) x) y | Eval.judgmentallyEqual x y -> pure (NaturalLit 0)
                    App (Var (V "Integer/clamp" 0)) (IntegerLit n)
                        | 0 <= n -> pure (NaturalLit (fromInteger n))
                        | otherwise -> pure (NaturalLit 0)
                    App (Var (V "Integer/negate" 0)) (IntegerLit n) ->
                        pure (IntegerLit (negate n))
                    App (Var (V "Integer/show" 0)) (IntegerLit n)
                        | 0 <= n    -> pure (TextLit (Chunks [] ("+" <> Text.pack (show n))))
                        | otherwise -> pure (TextLit (Chunks [] (Text.pack (show n))))
                    -- `(read . show)` is used instead of `fromInteger` because `read` uses
                    -- the correct rounding rule.
                    -- See https://gitlab.haskell.org/ghc/ghc/issues/17231.
                    App (Var (V "Integer/toDouble" 0)) (IntegerLit n) -> pure (DoubleLit ((DhallDouble . read . show) n))
                    App (Var (V "Double/show" 0)) (DoubleLit (DhallDouble n)) ->
                        pure (TextLit (Chunks [] (Text.pack (show n))))
                    App (App (Var (V "List/build" 0)) _A₀) g -> loop (App (App (App g list) cons) nil)
                      where
                        _A₁ = Syntax.shift 1 "a" _A₀

                        list = App List _A₀

                        cons =
                            Lam mempty (Syntax.makeFunctionBinding "a" _A₀)
                                (Lam mempty
                                    (Syntax.makeFunctionBinding "as" (App List _A₁))
                                    (ListAppend (ListLit Nothing (pure "a")) "as")
                                )

                        nil = ListLit (Just (App List _A₀)) empty
                    App (App (App (App (App (Var (V "List/fold" 0)) _) (ListLit _ xs)) t) cons) nil -> do
                      t' <- loop t
                      if boundedType t' then strict else lazy
                      where
                        strict =       foldr strictCons strictNil xs
                        lazy   = loop (foldr   lazyCons   lazyNil xs)

                        strictNil = loop nil
                        lazyNil   =      nil

                        strictCons y ys =
                          App (App cons y) <$> ys >>= loop
                        lazyCons   y ys =       App (App cons y) ys
                    App (App (Var (V "List/length" 0)) _) (ListLit _ ys) ->
                        pure (NaturalLit (fromIntegral (Data.Sequence.length ys)))
                    App (App (Var (V "List/head" 0)) t) (ListLit _ ys) -> loop o
                      where
                        o = case Data.Sequence.viewl ys of
                                y :< _ -> Some y
                                _      -> App None t
                    App (App (Var (V "List/last" 0)) t) (ListLit _ ys) -> loop o
                      where
                        o = case Data.Sequence.viewr ys of
                                _ :> y -> Some y
                                _      -> App None t
                    App (App (Var (V "List/indexed" 0)) _A₀) (ListLit _ as₀) -> loop (ListLit t as₁)
                      where
                        as₁ = Data.Sequence.mapWithIndex adapt as₀

                        _A₂ = Record (Dhall.Map.fromList kts)
                          where
                            kts = [ ("index", Syntax.makeRecordField Natural)
                                  , ("value", Syntax.makeRecordField _A₀)
                                  ]

                        t | null as₀  = Just (App List _A₂)
                          | otherwise = Nothing

                        adapt n a_ =
                            RecordLit (Dhall.Map.fromList kvs)
                          where
                            kvs = [ ("index", Syntax.makeRecordField $ NaturalLit (fromIntegral n))
                                  , ("value", Syntax.makeRecordField a_)
                                  ]
                    App (App (Var (V "List/reverse" 0)) _) (ListLit t xs) ->
                        loop (ListLit t (Data.Sequence.reverse xs))
                    App (Var (V "Text/show" 0)) (TextLit (Chunks [] oldText)) ->
                        loop (TextLit (Chunks [] newText))
                      where
                        newText = Eval.textShow oldText
                    App
                        (App (App (Var (V "Text/replace" 0)) (TextLit (Chunks [] ""))) _)
                        haystack ->
                            return haystack
                    App (App
                            (App (Var (V "Text/replace" 0)) (TextLit (Chunks [] needleText)))
                            replacement
                        )
                        (TextLit (Chunks [] lastText)) -> do
                            let (prefix, suffix) =
                                    Text.breakOn needleText lastText

                            if Text.null suffix
                                then return (TextLit (Chunks [] lastText))
                                else do
                                    let remainder =
                                            Text.drop
                                                (Text.length needleText)
                                                suffix

                                    loop (TextAppend (TextLit (Chunks [(prefix, replacement)] "")) (App (App (App (Var (V "Text/replace" 0)) (TextLit (Chunks [] needleText))) replacement) (TextLit (Chunks [] remainder))))
                    App (Var (V "Date/show" 0)) (DateLiteral date) ->
                        loop (TextLit (Chunks [] text))
                      where
                        text = Eval.dateShow date
                    App (Var (V "Time/show" 0)) (TimeLiteral hh mm ss frac precision) ->
                        loop (TextLit (Chunks [] text))
                      where
                        text = Eval.timeShow hh mm ss frac precision
                    App (Var (V "TimeZone/show" 0)) (TimeZoneLiteral timezone) ->
                        loop (TextLit (Chunks [] text))
                      where
                        text = Eval.timezoneShow timezone
                    _ -> do
                        res2 <- ctx (App f' a')
                        case res2 of
                            Nothing -> pure (App f' a')
                            Just app' -> loop app'
          Let (Binding _ f _ _ _ r) b -> loop b''
            where
              r'  = Syntax.shift   1  (V f 0) r
              b'  = subst (V f 0) r' b
              b'' = Syntax.shift (-1) (V f 0) b'
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
          Bytes -> pure Bytes
          BytesLit b -> pure (BytesLit b)
          Natural -> pure Natural
          NaturalLit n -> pure (NaturalLit n)
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
          Double -> pure Double
          DoubleLit n -> pure (DoubleLit n)
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
          Date ->
              pure Date
          DateLiteral d ->
              pure (DateLiteral d)
          Time ->
              pure Time
          TimeLiteral hh mm ss frac p ->
              pure (TimeLiteral hh mm ss frac p)
          TimeZone ->
              pure TimeZone
          TimeZoneLiteral z ->
              pure (TimeZoneLiteral z)
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
          Optional -> pure Optional
          Some a -> Some <$> a'
            where
              a' = loop a
          None -> pure None
          Record kts -> Record . Dhall.Map.sort <$> kts'
            where
              f (RecordField s0 expr s1 s2) = (\expr' -> RecordField s0 expr' s1 s2) <$> loop expr
              kts' = traverse f kts
          RecordLit kvs -> RecordLit . Dhall.Map.sort <$> kvs'
            where
              f (RecordField s0 expr s1 s2) = (\expr' -> RecordField s0 expr' s1 s2) <$> loop expr
              kvs' = traverse f kvs
          Union kts -> Union . Dhall.Map.sort <$> kts'
            where
              kts' = traverse (traverse loop) kts
          Combine cs mk x y -> decide <$> loop x <*> loop y
            where
              decide (RecordLit m) r | Data.Foldable.null m =
                  r
              decide l (RecordLit n) | Data.Foldable.null n =
                  l
              decide (RecordLit m) (RecordLit n) =
                  RecordLit (Dhall.Map.unionWith f m n)
                where
                  f (RecordField _ expr _ _) (RecordField _ expr' _ _) =
                    Syntax.makeRecordField $ decide expr expr'
              decide l r =
                  Combine cs mk l r
          CombineTypes cs x y -> decide <$> loop x <*> loop y
            where
              decide (Record m) r | Data.Foldable.null m =
                  r
              decide l (Record n) | Data.Foldable.null n =
                  l
              decide (Record m) (Record n) =
                  Record (Dhall.Map.unionWith f m n)
                where
                  f (RecordField _ expr _ _) (RecordField _ expr' _ _) =
                    Syntax.makeRecordField $ decide expr expr'
              decide l r =
                  CombineTypes cs l r
          Prefer cs _ x y -> decide <$> loop x <*> loop y
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
                  Prefer cs PreferFromSource l r
          RecordCompletion x y ->
              loop (Annot (Prefer mempty PreferFromCompletion (Field x def) y) (Field x typ))
            where
              def = Syntax.makeFieldSelection "default"
              typ = Syntax.makeFieldSelection "Type"
          Merge x y t      -> do
              x' <- loop x
              y' <- loop y
              case x' of
                  RecordLit kvsX ->
                      case y' of
                          Field (Union ktsY) (Syntax.fieldSelectionLabel -> kY) ->
                              case Dhall.Map.lookup kY ktsY of
                                  Just Nothing ->
                                      case recordFieldValue <$> Dhall.Map.lookup kY kvsX of
                                          Just vX -> return vX
                                          Nothing -> Merge x' y' <$> t'
                                  _ ->
                                      Merge x' y' <$> t'
                          App (Field (Union ktsY) (Syntax.fieldSelectionLabel -> kY)) vY ->
                              case Dhall.Map.lookup kY ktsY of
                                  Just (Just _) ->
                                      case recordFieldValue <$> Dhall.Map.lookup kY kvsX of
                                          Just vX -> loop (App vX vY)
                                          Nothing -> Merge x' y' <$> t'
                                  _ ->
                                      Merge x' y' <$> t'
                          Some a ->
                              case recordFieldValue <$> Dhall.Map.lookup "Some" kvsX of
                                  Just vX -> loop (App vX a)
                                  Nothing -> Merge x' y' <$> t'
                          App None _ ->
                              case recordFieldValue <$> Dhall.Map.lookup "None" kvsX of
                                  Just vX -> return vX
                                  Nothing -> Merge x' y' <$> t'
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
                                      [ ("mapKey"  , Syntax.makeRecordField $ TextLit (Chunks [] key))
                                      , ("mapValue", Syntax.makeRecordField value                  )
                                      ]
                                  )

                      let keyValues = Data.Sequence.fromList (map entry (Dhall.Map.toList $ recordFieldValue <$> kvsX))

                      let listType = case t' of
                              Just _ | null keyValues ->
                                  t'
                              _ ->
                                  Nothing

                      return (ListLit listType keyValues)
                  _ ->
                      return (ToMap x' t')
          ShowConstructor x -> do
              x' <- loop x
              return $ case x' of
                  Field (Union ktsY) (Syntax.fieldSelectionLabel -> kY) ->
                      case Dhall.Map.lookup kY ktsY of
                          Just Nothing -> TextLit (Chunks [] kY)
                          _ -> ShowConstructor x'
                  App (Field (Union ktsY) (Syntax.fieldSelectionLabel -> kY)) _ ->
                      case Dhall.Map.lookup kY ktsY of
                          Just (Just _) -> TextLit (Chunks [] kY)
                          _ -> ShowConstructor x'
                  Some _ ->
                      TextLit (Chunks [] "Some")
                  App None _ ->
                      TextLit (Chunks [] "None")
                  _ -> ShowConstructor x'
          Field r k@FieldSelection{fieldSelectionLabel = x}        -> do
              let singletonRecordLit v = RecordLit (Dhall.Map.singleton x v)

              r' <- loop r
              case r' of
                  RecordLit kvs ->
                      case Dhall.Map.lookup x kvs of
                          Just v  -> pure $ recordFieldValue v
                          Nothing -> Field <$> (RecordLit <$> traverse (Syntax.recordFieldExprs loop) kvs) <*> pure k
                  Project r_ _ -> loop (Field r_ k)
                  Prefer cs _ (RecordLit kvs) r_ -> case Dhall.Map.lookup x kvs of
                      Just v -> pure (Field (Prefer cs PreferFromSource (singletonRecordLit v) r_) k)
                      Nothing -> loop (Field r_ k)
                  Prefer _ _ l (RecordLit kvs) -> case Dhall.Map.lookup x kvs of
                      Just v -> pure $ recordFieldValue v
                      Nothing -> loop (Field l k)
                  Combine cs m (RecordLit kvs) r_ -> case Dhall.Map.lookup x kvs of
                      Just v -> pure (Field (Combine cs m (singletonRecordLit v) r_) k)
                      Nothing -> loop (Field r_ k)
                  Combine cs m l (RecordLit kvs) -> case Dhall.Map.lookup x kvs of
                      Just v -> pure (Field (Combine cs m l (singletonRecordLit v)) k)
                      Nothing -> loop (Field l k)
                  _ -> pure (Field r' k)
          Project x (Left fields)-> do
              x' <- loop x
              let fieldsSet = Data.Set.fromList fields
              case x' of
                  RecordLit kvs ->
                      pure (RecordLit (Dhall.Map.restrictKeys kvs fieldsSet))
                  Project y _ ->
                      loop (Project y (Left fields))
                  Prefer cs _ l (RecordLit rKvs) -> do
                      let rKs = Dhall.Map.keysSet rKvs
                      let l' = Project l (Left (Data.Set.toList (Data.Set.difference fieldsSet rKs)))
                      let r' = RecordLit (Dhall.Map.restrictKeys rKvs fieldsSet)
                      loop (Prefer cs PreferFromSource l' r')
                  _ | null fields -> pure (RecordLit mempty)
                    | otherwise   -> pure (Project x' (Left (Data.Set.toList (Data.Set.fromList fields))))
          Project r (Right e1) -> do
              e2 <- loop e1

              case e2 of
                  Record kts ->
                      loop (Project r (Left (Data.Set.toList (Dhall.Map.keysSet kts))))
                  _ -> do
                      r' <- loop r
                      pure (Project r' (Right e2))
          Assert t -> do
              t' <- loop t

              pure (Assert t')
          Equivalent cs l r -> do
              l' <- loop l
              r' <- loop r

              pure (Equivalent cs l' r')
          With r ks v -> do
              r' <- loop r
              v' <- loop v

              case r' of
                  RecordLit kvs ->
                      case ks of
                          WithLabel k :| [] ->
                              return (RecordLit (Dhall.Map.insert k (Syntax.makeRecordField v') kvs))
                          WithLabel k₀ :| k₁ : ks' -> do
                              let e₁ =
                                      case Dhall.Map.lookup k₀ kvs of
                                          Nothing  -> RecordLit mempty
                                          Just val -> Syntax.recordFieldValue val

                              e₂ <- loop (With e₁ (k₁ :| ks') v')

                              return (RecordLit (Dhall.Map.insert k₀ (Syntax.makeRecordField e₂) kvs))
                          WithQuestion :| _ -> do
                              return (With r' ks v')
                  Some t ->
                      case ks of
                          WithQuestion :| [] -> do
                              return (Some v')
                          WithQuestion :| k : ks' -> do
                              w <- loop (With t (k :| ks') v)
                              return (Some w)
                          WithLabel _ :| _ ->
                              return (With r' ks v')
                  App None _T ->
                      case ks of
                          WithQuestion :| _ ->
                              return (App None _T)
                          WithLabel _ :| _ ->
                              return (With r' ks v')
                  _ ->
                      return (With r' ks v')
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
isNormalized e =
    denote' e == denote' (normalize e)
  where
    denote' :: Expr t b -> Expr () b
    denote' = Syntax.denote

{-| Detect if the given variable is free within the given expression

>>> "x" `freeIn` "x"
True
>>> "x" `freeIn` "y"
False
>>> "x" `freeIn` Lam mempty (Syntax.makeFunctionBinding "x" (Const Type)) "x"
False
-}
freeIn :: Eq a => Var -> Expr s a -> Bool
variable@(V var i) `freeIn` expression =
    subst variable (Var (V var (i + 1))) strippedExpression
      /= strippedExpression
  where
    denote' :: Expr t b -> Expr () b
    denote' = Syntax.denote

    strippedExpression = denote' expression
{-# INLINABLE freeIn #-}

{- $setup
>>> import Dhall.Syntax (Const(..))
-}
