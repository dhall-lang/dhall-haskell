{-# language Strict, LambdaCase, OverloadedStrings, RankNTypes,
             ScopedTypeVariables, TupleSections, ViewPatterns,
             PatternSynonyms
             #-}

{-# options_ghc -O -Wincomplete-patterns -Wno-name-shadowing
                   -Wno-unused-matches #-}

{-|
Eval-apply environment machine with conversion checking and quoting to normal
forms. Fairly similar to GHCI's STG machine algorithmically, but much simpler,
with no known call optimization, forcing operations or environment trimming.

Features:

- No changes to Dhall core Expr.

- We can evaluate out-of-environment variables, in order to be compatible with
  the Dhall normalization tests. This is just a stopgap measure! The current
  implementation is only correct if unbound variables have unique non-shadowed
  names. I think it is generally a bad idea to have untyped free variables.

- Function application is call-by-need by default.

- Semantics of normalization follows old implementation, although asymptotics
  can be widly different.

- In contrast to Expr, we don't conflate Record and Union Fields in values.

- Environments are just association lists with linear lookup, no environment
  trimming.

- Currently, we check eta equality for functions, but not for records.

- Curried primops are eta-expanded into higher-order closures returning saturated
  primop applications. On quoting into normal forms, we eta-contract away
  the leftover higher-order binders.

- Build-fold fusion is implemented via annotated higher-order closures for fold primitives.

- Quoting performs alpha-normalization the same way as the old alphaNormalize function.
  There is no normalization currently which does not perform alpha-normalization. In
  a more auspicious implementation, Expr would have full de Bruijn indices at all times,
  making alpha normalization needless.

Potential optimizations without changing Expr:
  - In conversion checking, get non-shadowing variables not by linear
    Env-walking, but by keeping track of Env size, and generating names which
    are known to be illegal as source-level names (to rule out shadowing).
  - Use HashMap Text chunks for large let-definitions blocks. "Large" vs "Small"
    is fairly cheap to determine at evaluation time. 10+ sized blocks sound
    right for starters.

Potential optimizations with changing Expr:
  - Use Int in Var instead of Integer. Overflow is practically impossible.
  - Use actual full de Bruijn indices in Var instead of Text counting indices. Then, we'd
    switch to full de Bruijn levels in Val as well, and use proper constant time non-shadowing
    name generation.

Dhall notes:
  - Why is there an OptionalLit?

  - Union and Record fields should not be conflated. For unions, a better and
    more type-theory-compliant name would be "injection" or instead of "field".

  - UnionLit should be deprecated and removed.

  - Strictness:

    - boundedType is wrong for strictness. For example, "any" should be lazy,
      even though Bool is "bounded". In fact, it could be argued that Dhall
      should be call-by-value, because of:
        - the lack of corecursion
        - the noticeable performance boost of cbv in evaluation
        - the spine-strictness of the only available list type

      Assuming we keep "boundedType", functions should be also marked as
      bounded, because function closure sizes are bounded by the lexical depth
      of source programs, and are usually cheaper to compute than data like
      lists and numbers.

    - Fold strictness in any case should be chosen at compile time, not at
      runtime. At compile time everything is already known, the problem
      currently is that there is no elaboration at all, only type checking, so
      source syntax cannot be extended with new information. There should be a
      raw syntax separate from typed core syntax.

  - Usage of build fusion in Prelude seems to increase performance for the old
    normalizer, but since performance seems to be unacceptable with or without
    fusion, I wouldn't count this as a clear win for fusion. With the new
    normalizer, we get a large and sometimes asymptotic performance benefit from
    direct, non-fusing list function definitions. I find it likely that fusion
    should be altogether removed.

  - Conversion-checking based simplification rules for primitive operations are
    of dubious value; it might be the case that they have nontrivial performance
    cost; this should be benchmarked.

  - Type checking should be bidirectional even if we don't change anything about
    surface syntax, because it is faster and much clearer to implement.

-}

module Dhall.Eval (
    Env(..)
  , QEnv(..)
  , Closure
  , inst
  , clBinder
  , VChunks(..)
  , Val(..)
  , eval
  , conv
  , quote
  , qEnv
  , nf
  ) where

import Data.Foldable
import Data.List.NonEmpty hiding (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Dhall.Core
import Dhall.Map (Map)
import Dhall.Set (Set)
import GHC.Natural
import Unsafe.Coerce

import qualified Data.Char
import qualified Data.Sequence
import qualified Data.Text
import qualified Dhall.Map
import qualified Text.Printf

-- import qualified Dhall.Parser
-- import qualified Control.Exception
-- import qualified Dhall.Import
-- import qualified Dhall.TypeCheck
-- import Debug.Trace

----------------------------------------------------------------------------------------------------

data Env s a =
    Empty
  | Skip (Env s a) {-# unpack #-} Text
  | Extend (Env s a) {-# unpack #-} Text ~(Val s a)

data Void

-- | We unpack closures by unsafe coercion. We could have a GADT with an
--   existential "s" type for Expr, but that can't be unpacked. The performance
--   benefit from this is probably irrelevant; I was just annoyed to leave
--   it without unpacking.
data Closure s a = Cl_ Text (Env s a) (Expr Void a)

pattern Cl :: Text -> Env s a -> Expr s' a -> Closure s a
pattern Cl x env t <- Cl_ x env (unsafeCoerce -> t) where
  Cl x env t = Cl_ x env (unsafeCoerce t)
{-# complete Cl #-}

clBinder :: Closure s a -> Text
clBinder (Cl x _ _) = x
{-# inline clBinder #-}

data VChunks s a = VChunks [(Text, Val s a)] Text

instance Semigroup (VChunks s a) where
  VChunks xys z <> VChunks [] z' = VChunks xys (z <> z')
  VChunks xys z <> VChunks ((x', y'):xys') z' = VChunks (xys ++ (z <> x', y'):xys') z'

instance Monoid (VChunks s a) where
  mempty = VChunks [] mempty

data HLamInfo s a
  = Prim
  | Typed Text ~(Val s a)
  | NaturalFoldCl ~(Val s a)
  | ListFoldCl ~(Val s a) ~(Val s a)
  | OptionalFoldCl ~(Val s a) ~(Val s a)

pattern VPrim :: (Val s a -> Val s a) -> Val s a
pattern VPrim f = VHLam Prim f

data Val s a
  = VConst Const
  | VVar Text Int
  | VApp (Val s a) ~(Val s a)
  | VLam ~(Val s a) {-# unpack #-} (Closure s a)
  | VHLam (HLamInfo s a) (Val s a -> Val s a)
  | VPi  ~(Val s a) {-# unpack #-} (Closure s a)

  | VBool
  | VBoolLit Bool
  | VBoolAnd (Val s a) (Val s a)
  | VBoolOr (Val s a) (Val s a)
  | VBoolEQ (Val s a) (Val s a)
  | VBoolNE (Val s a) (Val s a)
  | VBoolIf (Val s a) (Val s a) (Val s a)

  | VNatural
  | VNaturalLit Natural
  | VNaturalFold (Val s a) (Val s a) ~(Val s a) ~(Val s a)
  | VNaturalBuild (Val s a)
  | VNaturalIsZero (Val s a)
  | VNaturalEven (Val s a)
  | VNaturalOdd (Val s a)
  | VNaturalToInteger (Val s a)
  | VNaturalShow (Val s a)
  | VNaturalPlus (Val s a) (Val s a)
  | VNaturalTimes (Val s a) (Val s a)

  | VInteger
  | VIntegerLit Integer
  | VIntegerShow (Val s a)
  | VIntegerToDouble (Val s a)

  | VDouble
  | VDoubleLit Double
  | VDoubleShow (Val s a)

  | VText
  | VTextLit (VChunks s a)
  | VTextAppend (Val s a) (Val s a)
  | VTextShow (Val s a)

  | VList ~(Val s a)
  | VListLit (Maybe (Val s a)) (Seq (Val s a))
  | VListAppend (Val s a) (Val s a)
  | VListBuild   ~(Val s a) (Val s a)
  | VListFold    ~(Val s a) (Val s a) ~(Val s a) ~(Val s a) ~(Val s a)
  | VListLength  ~(Val s a) (Val s a)
  | VListHead    ~(Val s a) (Val s a)
  | VListLast    ~(Val s a) (Val s a)
  | VListIndexed ~(Val s a) (Val s a)
  | VListReverse ~(Val s a) (Val s a)

  | VOptional ~(Val s a)
  | VSome ~(Val s a)
  | VNone ~(Val s a)
  | VOptionalFold ~(Val s a) (Val s a) ~(Val s a) ~(Val s a) ~(Val s a)
  | VOptionalBuild ~(Val s a) (Val s a)
  | VRecord (Map Text (Val s a))
  | VRecordLit (Map Text (Val s a))
  | VUnion (Map Text (Maybe (Val s a)))
  | VUnionLit Text ~(Val s a) (Map Text (Maybe (Val s a)))
  | VCombine (Val s a) (Val s a)
  | VCombineTypes (Val s a) (Val s a)
  | VPrefer (Val s a) (Val s a)
  | VMerge (Val s a) (Val s a) (Maybe (Val s a))
  | VField (Val s a) Text
  | VInject (Map Text (Maybe (Val s a))) Text (Maybe (Val s a))
  | VProject (Val s a) (Set Text)
  | VEmbed ~a


-- Evaluation
----------------------------------------------------------------------------------------------------

boundedType :: Val s a -> Bool
boundedType = \case
  VBool       -> True
  VNatural    -> True
  VInteger    -> True
  VDouble     -> True
  VText       -> True
  VPi{}       -> True    -- this is correct, although missing from the old normalizer
  VList _     -> False
  VOptional t -> boundedType t
  VRecord kvs -> all boundedType kvs
  VUnion kvs  -> all (all boundedType) kvs
  _           -> False

textShow :: Text -> Text
textShow text = "\"" <> Data.Text.concatMap f text <> "\""
  where
    f '"'  = "\\\""
    f '$'  = "\\u0024"
    f '\\' = "\\\\"
    f '\b' = "\\b"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f '\t' = "\\t"
    f '\f' = "\\f"
    f c | c <= '\x1F' = Data.Text.pack (Text.Printf.printf "\\u%04x" (Data.Char.ord c))
        | otherwise   = Data.Text.singleton c

countName :: Text -> Env s a -> Int
countName x = go (0 :: Int) where
  go acc Empty             = acc
  go acc (Skip env x'    ) = go (if x == x' then acc + 1 else acc) env
  go acc (Extend env x' _) = go (if x == x' then acc + 1 else acc) env

-- TODO: use HashMap chunks for large let blocks.
evalVar :: Env s a -> Var -> Val s a
evalVar env (V x i) = go env i where
  go (Extend env x' v) i
    | x == x'   = if i == 0 then v else go env (i - 1)
    | otherwise = go env i
  go (Skip env x') i
    | x == x'   = if i == 0 then VVar x (countName x env) else go env (i - 1)
    | otherwise = go env i
  go _ _ = VVar x (-1)

inst :: Eq a => Closure s a -> Val s a -> Val s a
inst (Cl x env t) ~u = eval (Extend env x u) t
{-# inline inst #-}

vApp :: Eq a => Val s a -> Val s a -> Val s a
vApp t ~u = case t of
  VLam _ t    -> inst t u
  VHLam _ t   -> t u
  t           -> VApp t u
{-# inline vApp #-}

vCombine :: Val s a -> Val s a -> Val s a
vCombine t u = case (t, u) of
  (VRecordLit m, u) | null m    -> u
  (t, VRecordLit m) | null m    -> t
  (VRecordLit m, VRecordLit m') -> VRecordLit (Dhall.Map.sort (Dhall.Map.unionWith vCombine m m'))
  (t, u)                        -> VCombine t u
{-# inline vCombine #-}

vCombineTypes :: Val s a -> Val s a -> Val s a
vCombineTypes t u = case (t, u) of
  (VRecord m, u) | null m -> u
  (t, VRecord m) | null m -> t
  (VRecord m, VRecord m') -> VRecord (Dhall.Map.sort (Dhall.Map.unionWith vCombineTypes m m'))
  (t, u)                  -> VCombineTypes t u
{-# inline vCombineTypes #-}

vListAppend :: Val s a -> Val s a -> Val s a
vListAppend t u = case (t, u) of
  (VListLit _ xs, u) | null xs   -> u
  (t, VListLit _ ys) | null ys   -> t
  (VListLit t xs, VListLit _ ys) -> VListLit t (xs <> ys)
  (t, u)                         -> VListAppend t u
{-# inline vListAppend #-}

vNaturalPlus :: Val s a -> Val s a -> Val s a
vNaturalPlus t u = case (t, u) of
  (VNaturalLit 0, u            ) -> u
  (t,             VNaturalLit 0) -> t
  (VNaturalLit m, VNaturalLit n) -> VNaturalLit (m + n)
  (t,             u            ) -> VNaturalPlus t u
{-# inline vNaturalPlus #-}

vBuild :: Eq a => Val s a -> Val s a -> (Val s a -> Val s a) -> (Val s a -> Val s a) -> Val s a
vBuild t ~u canonical neutral = case t of
  VLam _ t  -> canonical (inst t u)
  VHLam _ t -> canonical (t u)
  t         -> neutral t
{-# inline vBuild #-}

eval :: forall s s' a. Eq a => Env s a -> Expr s' a -> Val s a
eval env t =
  let
    evalE :: Expr s' a -> Val s a
    evalE = eval env
    {-# inline evalE #-}

    evalChunks :: Chunks s' a -> VChunks s a
    evalChunks (Chunks xys z) =
      foldr (\(x, t) vcs ->
                case evalE t of
                  VTextLit vcs' -> VChunks [] x <> vcs' <> vcs
                  t             -> VChunks [(x, t)] mempty <> vcs)
            (VChunks [] z)
            xys
    {-# inline evalChunks #-}

  in case t of
    Const k          -> VConst k
    Var v            -> evalVar env v
    Lam x a t        -> VLam (evalE a) (Cl x env t)
    Pi x a b         -> VPi (evalE a) (Cl x env b)
    App t u          -> vApp (evalE t) (evalE u)
    Let (b :| bs) t  -> go env (b:bs) where
                          go env []     = eval env t
                          go env (b:bs) = go (Extend env (variable b)
                                                         (eval env (value b))) bs
    Annot t _        -> evalE t

    Bool             -> VBool
    BoolLit b        -> VBoolLit b
    BoolAnd t u      -> case (evalE t, evalE u) of
                          (VBoolLit True, u)    -> u
                          (VBoolLit False, u)   -> VBoolLit False
                          (t, VBoolLit True)    -> t
                          (t, VBoolLit False)   -> VBoolLit False
                          (t, u) | conv env t u -> t
                          (t, u)                -> VBoolAnd t u
    BoolOr t u       -> case (evalE t, evalE u) of
                          (VBoolLit False, u)   -> u
                          (VBoolLit True, u)    -> VBoolLit True
                          (t, VBoolLit False)   -> t
                          (t, VBoolLit True)    -> VBoolLit True
                          (t, u) | conv env t u -> t
                          (t, u)                -> VBoolOr t u
    BoolEQ t u       -> case (evalE t, evalE u) of
                          (VBoolLit True, u)    -> u
                          (t, VBoolLit True)    -> t
                          (t, u) | conv env t u -> VBoolLit True
                          (t, u)                -> VBoolEQ t u
    BoolNE t u       -> case (evalE t, evalE u) of
                          (VBoolLit False, u)   -> u
                          (t, VBoolLit False)   -> t
                          (t, u) | conv env t u -> VBoolLit False
                          (t, u)                -> VBoolNE t u
    BoolIf b t f     -> case (evalE b, evalE t, evalE f) of
                          (VBoolLit True,  t, f)   -> t
                          (VBoolLit False, t, f)   -> f
                          (b, VBoolLit True, VBoolLit False) -> b
                          (b, t, f) | conv env t f -> t
                          (b, t, f)                -> VBoolIf b t f

    Natural          -> VNatural
    NaturalLit n     -> VNaturalLit n
    NaturalFold      -> VPrim $ \ ~n -> VHLam (NaturalFoldCl n) $ \a ->
                          if boundedType a then
                            VPrim $ \s -> VPrim $ \z ->
                              let go acc 0 = acc
                                  go acc n = go (vApp s acc) (n - 1)
                              in case n of
                                   VNaturalLit n -> go z n
                                   n             -> VNaturalFold n a s z
                          else
                            VPrim $ \ ~s -> VPrim $ \ ~z ->
                              let go 0 = z
                                  go n = vApp s (go (n - 1))
                              in case n of
                                   VNaturalLit n -> go n
                                   n             -> VNaturalFold n a s z
    NaturalBuild     -> VPrim $ \case
                          VHLam (NaturalFoldCl x) _ -> x
                          t -> vBuild t VNatural
                            (\t -> t `vApp` VHLam (Typed "n" VNatural) (\n -> vNaturalPlus n (VNaturalLit 1))
                                     `vApp` VNaturalLit 0)
                            VNaturalBuild
    NaturalIsZero    -> VPrim $ \case VNaturalLit n -> VBoolLit (n == 0)
                                      n             -> VNaturalIsZero n
    NaturalEven      -> VPrim $ \case VNaturalLit n -> VBoolLit (even n)
                                      n             -> VNaturalEven n
    NaturalOdd       -> VPrim $ \case VNaturalLit n -> VBoolLit (odd n)
                                      n             -> VNaturalOdd n
    NaturalToInteger -> VPrim $ \case VNaturalLit n -> VIntegerLit (fromIntegral n)
                                      n             -> VNaturalToInteger n
    NaturalShow      -> VPrim $ \case VNaturalLit n -> VTextLit (VChunks [] (Data.Text.pack (show n)))
                                      n             -> VNaturalShow n
    NaturalPlus t u  -> vNaturalPlus (evalE t) (evalE u)
    NaturalTimes t u -> case (evalE t, evalE u) of
                          (VNaturalLit 1, u            ) -> u
                          (t,             VNaturalLit 1) -> t
                          (VNaturalLit 0, u            ) -> VNaturalLit 0
                          (t,             VNaturalLit 0) -> VNaturalLit 0
                          (VNaturalLit m, VNaturalLit n) -> VNaturalLit (m * n)
                          (t,             u            ) -> VNaturalTimes t u

    Integer          -> VInteger
    IntegerLit n     -> VIntegerLit n
    IntegerShow      -> VPrim $ \case
                          VIntegerLit n
                            | 0 <= n    -> VTextLit (VChunks [] (Data.Text.pack ('+':show n)))
                            | otherwise -> VTextLit (VChunks [] (Data.Text.pack (show n)))
                          n -> VIntegerShow n
    IntegerToDouble  -> VPrim $ \case VIntegerLit n -> VDoubleLit (read (show n))
                                      -- `(read . show)` is used instead of `fromInteger`
                                      -- because `read` uses the correct rounding rule
                                      n             -> VIntegerToDouble n

    Double           -> VDouble
    DoubleLit n      -> VDoubleLit n
    DoubleShow       -> VPrim $ \case VDoubleLit n -> VTextLit (VChunks [] (Data.Text.pack (show n)))
                                      n            -> VDoubleShow n

    Text             -> VText
    TextLit cs       -> case evalChunks cs of
                          VChunks [("", t)] "" -> t
                          vcs                  -> VTextLit vcs
    TextAppend t u   -> case (evalE t, evalE u) of
                          (VTextLit (VChunks [] ""), u) -> u
                          (t, VTextLit (VChunks [] "")) -> t
                          (VTextLit x, VTextLit y)      -> VTextLit (x <> y)
                          (t, u)                        -> VTextAppend t u
    TextShow         -> VPrim $ \case
                          VTextLit (VChunks [] x) -> VTextLit (VChunks [] (textShow x))
                          t                       -> VTextShow t

    List             -> VPrim $ \ ~a -> VList a
    ListLit ma ts    -> VListLit (evalE <$> ma) (evalE <$> ts)
    ListAppend t u   -> vListAppend (evalE t) (evalE u)
    ListBuild        -> VPrim $ \ ~a -> VPrim $ \case
                          VHLam (ListFoldCl _ x) _ -> x
                          t -> vBuild t (VList a)
                            (\t -> t `vApp` VHLam (Typed "a" a) (\a ->
                                              VHLam (Typed "as" (VList a)) (\as ->
                                                vListAppend (VListLit Nothing (pure a)) as))
                                     `vApp` VListLit (Just a) mempty)
                            (VListBuild a)
    ListFold         -> VPrim $ \ ~a -> VPrim $ \ ~as -> VHLam (ListFoldCl a as) $ \t ->
                          if boundedType t then
                            VPrim $ \c -> VPrim $ \n ->
                              case as of
                                VListLit _ as -> foldl' (\b x -> c `vApp` x `vApp` b) n
                                                        (Data.Sequence.reverse as)
                                as -> VListFold a as t c n
                          else
                            VPrim $ \ ~c -> VPrim $ \ ~n ->
                              case as of
                                VListLit _ as -> foldr (\x b -> c `vApp` x `vApp` b) n as
                                as -> VListFold a as t c n
    ListLength       -> VPrim $ \ ~a -> VPrim $ \case
                          VListLit _ as -> VNaturalLit (fromIntegral (Data.Sequence.length as))
                          as            -> VListLength a as
    ListHead         -> VPrim $ \ ~a -> VPrim $ \case
                          VListLit _ as -> case Data.Sequence.viewl as of
                                             y Data.Sequence.:< _ -> VSome y
                                             _                    -> VNone a
                          as            -> VListHead a as
    ListLast         -> VPrim $ \ ~a -> VPrim $ \case
                          VListLit _ as -> case Data.Sequence.viewr as of
                                             _ Data.Sequence.:> t -> VSome t
                                             _                    -> VNone a
                          as            -> VListLast a as
    ListIndexed      -> VPrim $ \ ~a -> VPrim $ \case
                          VListLit _ as -> let
                            a' = if null as then
                                   Just (VRecord (Dhall.Map.fromList
                                                  [("index", VNatural), ("value", a)]))
                                 else
                                   Nothing
                            as' = Data.Sequence.mapWithIndex
                                    (\i t -> VRecordLit
                                      (Dhall.Map.fromList [("index", VNaturalLit (fromIntegral i)),
                                                           ("value", t)]))
                                    as
                            in VListLit a' as'
                          t -> VListIndexed a t
    ListReverse      -> VPrim $ \ ~a -> VPrim $ \case
                          VListLit t as | null as -> VListLit t (Data.Sequence.reverse as)
                          VListLit t as -> VListLit Nothing (Data.Sequence.reverse as)
                          t             -> VListReverse a t

    Optional         -> VPrim $ \ ~a -> VOptional a
    OptionalLit a mt -> maybe (VNone (evalE a)) (\t -> VSome (evalE t)) mt
    Some t           -> VSome (evalE t)
    None             -> VPrim $ \ ~a -> VNone a
    OptionalFold     -> VPrim $ \ ~a -> VPrim $ \ ~opt -> VHLam (OptionalFoldCl a opt) $ \ ~b ->
                        VPrim $ \ ~s -> VPrim $ \n -> case opt of
                          VNone _ -> n
                          VSome t -> s `vApp` t
                          opt     -> VOptionalFold a opt b s n
    OptionalBuild    -> VPrim $ \ ~a -> VPrim $ \case
                          VHLam (OptionalFoldCl _ x) _ -> x
                          t -> vBuild t (VOptional a)
                            (\t -> t `vApp` VHLam (Typed "a" a) VSome
                                     `vApp` VNone a)
                            (VOptionalBuild a)

    Record kts       -> VRecord (evalE <$> kts)
    RecordLit kts    -> VRecordLit (Dhall.Map.sort (evalE <$> kts))
    Union kts        -> VUnion (Dhall.Map.sort ((evalE <$>) <$> kts))
    UnionLit k v kts -> VUnionLit k (evalE v) (Dhall.Map.sort ((evalE <$>) <$> kts))
    Combine t u      -> vCombine (evalE t) (evalE u)
    CombineTypes t u -> vCombineTypes (evalE t) (evalE u)
    Prefer t u       -> case (evalE t, evalE u) of
                          (VRecordLit m, u) | null m -> u
                          (t, VRecordLit m) | null m -> t
                          (VRecordLit m, VRecordLit m') ->
                             VRecordLit (Dhall.Map.sort (Dhall.Map.union m' m))
                          (t, u) -> VPrefer t u
    Merge x y ma     -> case (evalE x, evalE y, evalE <$> ma) of
                          (VRecordLit m, VUnionLit k v _, _)
                            | Just f <- Dhall.Map.lookup k m -> f `vApp` v
                            | otherwise -> error "eval: impossible"
                          (VRecordLit m, VInject _ k mt, _)
                            | Just f  <- Dhall.Map.lookup k m -> maybe f (vApp f) mt
                            | otherwise -> error "eval: impossible"
                          (x, y, ma) -> VMerge x y ma
    Field t k        -> case evalE t of
                          VRecordLit m
                            | Just v <- Dhall.Map.lookup k m -> v
                            | otherwise -> error "eval: impossible"
                          VUnion m -> case Dhall.Map.lookup k m of
                            Just (Just _) -> VPrim $ \ ~u -> VInject m k (Just u)
                            Just Nothing  -> VInject m k Nothing
                            _             -> error "eval: impossible"
                          t -> VField t k
    Project t ks     -> if null ks then
                          VRecordLit mempty
                        else case evalE t of
                          VRecordLit kvs
                            | Just s <- traverse (\k -> (k,) <$> Dhall.Map.lookup k kvs) (toList ks)
                              -> VRecordLit (Dhall.Map.sort (Dhall.Map.fromList s))
                            | otherwise -> error "eval: impossible"
                          t -> VProject t ks
    Note _ e         -> evalE e
    ImportAlt t _    -> evalE t
    Embed a          -> VEmbed a


-- Conversion checking
--------------------------------------------------------------------------------

eqListBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqListBy f = go where
  go (x:xs) (y:ys) | f x y = go xs ys
  go [] [] = True
  go _  _  = False
{-# inline eqListBy #-}

eqMaybeBy :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybeBy f = go where
  go (Just x) (Just y) = f x y
  go Nothing  Nothing  = True
  go _        _        = False
{-# inline eqMaybeBy #-}

conv :: forall a s. Eq a => Env s a -> Val s a -> Val s a -> Bool
conv env t t' =
  let
    fresh :: Text -> (Text, Val s a)
    fresh x = (x, VVar x (countName x env))
    {-# inline fresh #-}

    freshCl :: Closure s a -> (Text, Val s a, Closure s a)
    freshCl cl@(Cl x _ _) = (x, snd (fresh x), cl)
    {-# inline freshCl #-}

    convChunks :: VChunks s a -> VChunks s a -> Bool
    convChunks (VChunks xys z) (VChunks xys' z') =
      eqListBy (\(x, y) (x', y') -> x == x' && conv env y y') xys xys' && z == z'
    {-# inline convChunks #-}

    convE :: Val s a -> Val s a -> Bool
    convE = conv env
    {-# inline convE #-}

    convSkip :: Text -> Val s a -> Val s a -> Bool
    convSkip x = conv (Skip env x)
    {-# inline convSkip #-}

  in case (t, t') of
    (VConst k, VConst k') -> k == k'
    (VVar x i, VVar x' i') -> x == x' && i == i'

    (VLam _ (freshCl -> (x, v, t)), VLam _ t' ) -> convSkip x (inst t v) (inst t' v)
    (VLam _ (freshCl -> (x, v, t)), VHLam _ t') -> convSkip x (inst t v) (t' v)
    (VLam _ (freshCl -> (x, v, t)), t'        ) -> convSkip x (inst t v) (vApp t' v)
    (VHLam _ t, VLam _ (freshCl -> (x, v, t'))) -> convSkip x (t v) (inst t' v)
    (VHLam _ t, VHLam _ t'                    ) -> let (x, v) = fresh "x" in convSkip x (t v) (t' v)
    (VHLam _ t, t'                            ) -> let (x, v) = fresh "x" in convSkip x (t v) (vApp t' v)

    (t, VLam _ (freshCl -> (x, v, t'))) -> convSkip x (vApp t v) (inst t' v)
    (t, VHLam _ t'  ) -> let (x, v) = fresh "x" in convSkip x (vApp t v) (t' v)

    (VPi a b, VPi a' (freshCl -> (x, v, b'))) ->
      convE a a' && convSkip x (inst b v) (inst b' v)

    (VBool       , VBool            ) -> True
    (VBoolLit b  , VBoolLit b'      ) -> b == b'
    (VBoolAnd t u, VBoolAnd t' u'   ) -> convE t t' && convE u u'
    (VBoolOr  t u, VBoolOr  t' u'   ) -> convE t t' && convE u u'
    (VBoolEQ  t u, VBoolEQ  t' u'   ) -> convE t t' && convE u u'
    (VBoolNE  t u, VBoolNE  t' u'   ) -> convE t t' && convE u u'
    (VBoolIf t u v, VBoolIf t' u' v') -> convE t t' && convE u u' && convE v v'

    (VNatural, VNatural) -> True
    (VNaturalLit n, VNaturalLit n') -> n == n'
    (VNaturalFold t _ u v, VNaturalFold t' _ u' v') ->
      convE t t' && convE u u' && convE v v'

    (VNaturalBuild t     , VNaturalBuild t')     -> convE t t'
    (VNaturalIsZero t    , VNaturalIsZero t')    -> convE t t'
    (VNaturalEven t      , VNaturalEven t')      -> convE t t'
    (VNaturalOdd t       , VNaturalOdd t')       -> convE t t'
    (VNaturalToInteger t , VNaturalToInteger t') -> convE t t'
    (VNaturalShow t      , VNaturalShow t')      -> convE t t'
    (VNaturalPlus t u    , VNaturalPlus t' u')   -> convE t t' && convE u u'
    (VNaturalTimes t u   , VNaturalTimes t' u')  -> convE t t' && convE u u'

    (VInteger           , VInteger)            -> True
    (VIntegerLit t      , VIntegerLit t')      -> t == t'
    (VIntegerShow t     , VIntegerShow t')     -> convE t t'
    (VIntegerToDouble t , VIntegerToDouble t') -> convE t t'

    (VDouble       , VDouble)        -> True
    (VDoubleLit n  , VDoubleLit n')  -> n == n'
    (VDoubleShow t , VDoubleShow t') -> convE t t'

    (VText, VText) -> True

    (VTextLit cs     , VTextLit cs')      -> convChunks cs cs'
    (VTextAppend t u , VTextAppend t' u') -> convE t t' && convE u u'
    (VTextShow t     , VTextShow t')      -> convE t t'

    (VList a        , VList a'      ) -> convE a a'
    (VListLit _ xs  , VListLit _ xs') -> eqListBy convE (toList xs) (toList xs')

    (VListAppend t u     , VListAppend t' u'       ) -> convE t t' && convE u u'
    (VListBuild a t      , VListBuild a' t'        ) -> convE t t'
    (VListLength a t     , VListLength a' t'       ) -> convE a a' && convE t t'
    (VListHead _ t       , VListHead _ t'          ) -> convE t t'
    (VListLast _ t       , VListLast _ t'          ) -> convE t t'
    (VListIndexed _ t    , VListIndexed _ t'       ) -> convE t t'
    (VListReverse _ t    , VListReverse _ t'       ) -> convE t t'
    (VListFold a l _ t u , VListFold a' l' _ t' u' ) ->
      convE a a' && convE l l' && convE t t' && convE u u'

    (VOptional a             , VOptional a'                ) -> convE a a'
    (VSome t                 , VSome t'                    ) -> convE t t'
    (VNone _                 , VNone _                     ) -> True
    (VOptionalBuild _ t      , VOptionalBuild _ t'         ) -> convE t t'
    (VRecord m               , VRecord m'                  ) -> eqListBy convE (toList m) (toList m')
    (VRecordLit m            , VRecordLit m'               ) -> eqListBy convE (toList m) (toList m')
    (VUnion m                , VUnion m'                   ) -> eqListBy (eqMaybeBy convE) (toList m) (toList m')
    (VUnionLit k v m         , VUnionLit k' v' m'          ) -> k == k' && convE v v' &&
                                                                  eqListBy (eqMaybeBy convE) (toList m) (toList m')
    (VCombine t u            , VCombine t' u'              ) -> convE t t' && convE u u'
    (VCombineTypes t u       , VCombineTypes t' u'         ) -> convE t t' && convE u u'
    (VPrefer  t u            , VPrefer t' u'               ) -> convE t t' && convE u u'
    (VMerge t u _            , VMerge t' u' _              ) -> convE t t' && convE u u'
    (VField t k              , VField t' k'                ) -> convE t t' && k == k'
    (VProject t ks           , VProject t' ks'             ) -> convE t t' && ks == ks'
    (VInject m k mt          , VInject m' k' mt'           ) -> eqListBy (eqMaybeBy convE) (toList m) (toList m')
                                                                  && k == k' && eqMaybeBy convE mt mt'
    (VEmbed a                , VEmbed a'                   ) -> a == a'
    (VOptionalFold a t _ u v , VOptionalFold a' t' _ u' v' ) ->
      convE a a' && convE t t' && convE u u' && convE v v'

    (_, _) -> False


-- Quoting
----------------------------------------------------------------------------------------------------

data QEnv
  = QEmpty
  | QBind QEnv {-# unpack #-} Text
  | QPrim QEnv {-# unpack #-} Text
  deriving Show

qEnv :: Env s a -> QEnv
qEnv Empty            = QEmpty
qEnv (Skip env x)     = QBind (qEnv env) x
qEnv (Extend env x _) = QBind (qEnv env) x

envLen :: Env s a -> Int
envLen = go 0 where
  go acc Empty            = acc
  go acc (Skip env _)     = go (acc + 1) env
  go acc (Extend env _ _) = go (acc + 1) env

-- countQName :: Text -> QEnv -> Int
-- countQName x = go 0 where
--   go acc QEmpty         = acc
--   go acc (QBind env x') = go (if x == x' then acc + 1 else acc) env
--   go acc (QPrim env x') = go (if x == x' then acc + 1 else acc) env

varIsPrim :: QEnv -> Text -> Int -> Bool
varIsPrim (QBind env x') x i
  | x == x'   = if i == 0 then False else varIsPrim env x (i - 1)
  | otherwise = varIsPrim env x i
varIsPrim (QPrim env x') x i
  | x == x'   = if i == 0 then True else varIsPrim env x (i - 1)
  | otherwise = varIsPrim env x i
varIsPrim _ _ _ = False

quote :: forall s a. Eq a => QEnv -> Int -> Val s a -> Expr s a
quote env l t =
  let
    -- fresh :: Text -> (Text, Val s a)
    -- fresh x = (x, VVar x (countQName x env))
    -- {-# inline fresh #-}

    -- freshCl :: Closure s a -> (Text, Val s a, Closure s a)
    -- freshCl cl@(Cl x _ _) = (x, snd (fresh x), cl)
    -- {-# inline freshCl #-}

    -- qVar :: Text -> Int -> Expr s a
    -- qVar x i = Var (V x (fromIntegral (countQName x env - i - 1)))
    -- {-# inline qVar #-}

    fresh :: Text -> (Text, Val s a)
    fresh x = ("_", VVar "_" l)
    {-# inline fresh #-}

    freshCl :: Closure s a -> (Text, Val s a, Closure s a)
    freshCl cl@(Cl x _ _) = ("_", snd (fresh x), cl)
    {-# inline freshCl #-}

    qVar :: Text -> Int -> Expr s a
    qVar x i = Var (V x (fromIntegral (l - i - 1)))
    {-# inline qVar #-}

    quoteE :: Val s a -> Expr s a
    quoteE = quote env l
    {-# inline quoteE #-}

    quoteBind :: Text -> Val s a -> Expr s a
    quoteBind x = quote (QBind env x) (l + 1)
    {-# inline quoteBind #-}

    qApp :: Expr s a -> Val s a -> Expr s a
    qApp t (VVar x i) | varIsPrim env x i = t
    qApp t u = App t (quoteE u)
    {-# inline qApp #-}

  in case t of
    VConst k                      -> Const k
    VVar x (-1)                   -> Var (V x 0)
    VVar x i                      -> qVar x i
    VApp t u                      -> quoteE t `qApp` u

    VLam a (freshCl -> (x, v, t)) -> Lam x (quoteE a) (quoteBind x (inst t v))
    VHLam i t                     -> case i of
                                       Prim -> let (x, v) = fresh "x" in quote (QPrim env x) (l + 1) (t v)
                                       Typed (fresh -> (x, v)) a -> Lam x (quoteE a) (quoteBind x (t v))
                                       NaturalFoldCl t           -> NaturalFold `qApp` t
                                       ListFoldCl a t            -> ListFold `qApp` a `qApp` t
                                       OptionalFoldCl a t        -> OptionalFold `qApp` a `qApp` t

    VPi a (freshCl -> (x, v, b))  -> Pi x (quoteE a) (quoteBind x (inst b v))

    VBool                         -> Bool
    VBoolLit b                    -> BoolLit b
    VBoolAnd t u                  -> BoolAnd (quoteE t) (quoteE u)
    VBoolOr t u                   -> BoolOr (quoteE t) (quoteE u)
    VBoolEQ t u                   -> BoolEQ (quoteE t) (quoteE u)
    VBoolNE t u                   -> BoolNE (quoteE t) (quoteE u)
    VBoolIf t u v                 -> BoolIf (quoteE t) (quoteE u) (quoteE v)

    VNatural                      -> Natural
    VNaturalLit n                 -> NaturalLit n
    VNaturalFold a t u v          -> NaturalFold `qApp` a `qApp` t `qApp` u `qApp` v
    VNaturalBuild t               -> NaturalBuild `qApp` t
    VNaturalIsZero t              -> NaturalIsZero `qApp` t
    VNaturalEven t                -> NaturalEven `qApp` t
    VNaturalOdd t                 -> NaturalOdd `qApp` t
    VNaturalToInteger t           -> NaturalToInteger `qApp` t
    VNaturalShow t                -> NaturalShow `qApp` t
    VNaturalPlus t u              -> NaturalPlus (quoteE t) (quoteE u)
    VNaturalTimes t u             -> NaturalTimes (quoteE t) (quoteE u)

    VInteger                      -> Integer
    VIntegerLit n                 -> IntegerLit n
    VIntegerShow t                -> IntegerShow `qApp` t
    VIntegerToDouble t            -> IntegerToDouble `qApp` t

    VDouble                       -> Double
    VDoubleLit n                  -> DoubleLit n
    VDoubleShow t                 -> DoubleShow `qApp` t

    VText                         -> Text
    VTextLit (VChunks xys z)      -> TextLit (Chunks ((quoteE <$>) <$> xys) z)
    VTextAppend t u               -> TextAppend (quoteE t) (quoteE u)
    VTextShow t                   -> TextShow `qApp` t

    VList t                       -> List `qApp` t
    VListLit ma ts                -> ListLit (quoteE <$> ma) (quoteE <$> ts)
    VListAppend t u               -> ListAppend (quoteE t) (quoteE u)
    VListBuild a t                -> ListBuild `qApp` a `qApp` t
    VListFold a l t u v           -> ListFold `qApp` a `qApp` l `qApp` t `qApp` u `qApp` v
    VListLength a t               -> ListLength `qApp` a `qApp` t
    VListHead a t                 -> ListHead `qApp` a `qApp` t
    VListLast a t                 -> ListLast `qApp` a `qApp` t
    VListIndexed a t              -> ListIndexed `qApp` a `qApp` t
    VListReverse a t              -> ListReverse `qApp` a `qApp` t

    VOptional a                   -> Optional `qApp` a
    VSome t                       -> Some (quoteE t)
    VNone t                       -> None `qApp` t
    VOptionalFold a o t u v       -> OptionalFold `qApp` a `qApp` o `qApp` t `qApp` u `qApp` v
    VOptionalBuild a t            -> OptionalBuild `qApp` a `qApp` t
    VRecord m                     -> Record (quoteE <$> m)
    VRecordLit m                  -> RecordLit (quoteE <$> m)
    VUnion m                      -> Union ((quoteE <$>) <$> m)
    VUnionLit k v m               -> UnionLit k (quoteE v) ((quoteE <$>) <$> m)
    VCombine t u                  -> Combine (quoteE t) (quoteE u)
    VCombineTypes t u             -> CombineTypes (quoteE t) (quoteE u)
    VPrefer t u                   -> Prefer (quoteE t) (quoteE u)
    VMerge t u ma                 -> Merge (quoteE t) (quoteE u) (quoteE <$> ma)
    VField t k                    -> Field (quoteE t) k
    VProject t ks                 -> Project (quoteE t) ks
    VInject m k Nothing           -> Field (Union ((quoteE <$>) <$> m)) k
    VInject m k (Just t)          -> Field (Union ((quoteE <$>) <$> m)) k `qApp` t
    VEmbed a                      -> Embed a

-- Normalization
----------------------------------------------------------------------------------------------------

nf :: Eq a => Env s a -> Expr s' a -> Expr s a
nf env = quote (qEnv env) (envLen env) . eval env


-- Quick & dirty testing
----------------------------------------------------------------------------------------------------

-- throws :: Control.Exception.Exception e => Either e a -> IO a
-- throws = either Control.Exception.throwIO pure

-- test :: Text -> IO ()
-- test t = do
--   raw      <- throws (Dhall.Parser.exprFromText "testing" t)
--   resolved <- Dhall.Import.assertNoImports raw
--   exp      <- resolved <$ throws (Dhall.TypeCheck.typeOf resolved)
--   -- print (Dhall.Core.denote exp :: Expr Dhall.Parser.Src Dhall.TypeCheck.X)
--   let norm = nf Empty resolved :: Expr Dhall.Parser.Src Dhall.TypeCheck.X
--   print norm
