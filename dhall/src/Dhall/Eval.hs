{-# LANGUAGE
  BangPatterns,
  CPP,
  LambdaCase,
  OverloadedStrings,
  PatternSynonyms,
  RankNTypes,
  ScopedTypeVariables,
  TupleSections,
  ViewPatterns
  #-}

{-# OPTIONS_GHC
  -O
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  #-}

{-|
Eval-apply environment machine with conversion checking and quoting to normal
forms. Fairly similar to GHCI's STG machine algorithmically, but much simpler,
with no known call optimization or environment trimming.

Potential optimizations without changing Expr:
  - In conversion checking, get non-shadowing variables not by linear
    Env-walking, but by keeping track of Env size, and generating names which
    are known to be illegal as source-level names (to rule out shadowing).
  - Use HashMap Text chunks for large let-definitions blocks. "Large" vs "Small"
    is fairly cheap to determine at evaluation time.

Potential optimizations with changing Expr:
  - Use Int in Var instead of Integer. Overflow is practically impossible.
  - Use actual full de Bruijn indices in Var instead of Text counting indices. Then, we'd
    switch to full de Bruijn levels in Val as well, and use proper constant time non-shadowing
    name generation.

-}

module Dhall.Eval (
    Env(..)
  , Names(..)
  , Closure(..)
  , VChunks(..)
  , Val(..)
  , Void
  , inst
  , eval
  , conv
  , convEmpty
  , quote
  , nf
  , nfEmpty
  , Dhall.Eval.alphaNormalize
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif

import Data.Foldable (foldr', foldl', toList)
import Data.List.NonEmpty (NonEmpty(..), cons)
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq)
import Data.Text (Text)

import Dhall.Core (
    Expr(..)
  , Binding(..)
  , Chunks(..)
  , Const(..)
  , Import
  , Var(..)
  , denote
  )

-- import Control.Exception (throw)
-- import Dhall.Import.Types (InternalError)
import Dhall.Map (Map)
import Dhall.Set (Set)
import GHC.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Char
import qualified Data.List.NonEmpty
import qualified Data.Sequence
import qualified Data.Text
import qualified Dhall.Binary
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Text.Printf

----------------------------------------------------------------------------------------------------

data Env a =
    Empty
  | Skip !(Env a) {-# unpack #-} !Text
  | Extend !(Env a) {-# unpack #-} !Text (Val a)

data Void

coeExprVoid :: Expr Void a -> Expr s a
coeExprVoid = unsafeCoerce
{-# inline coeExprVoid #-}

errorMsg :: String
errorMsg = unlines
  [ _ERROR <> ": Compiler bug                                                        "
  , "                                                                                "
  , "An ill-typed expression was encountered during normalization.                   "
  , "Explanation: This error message means that there is a bug in the Dhall compiler."
  , "You didn't do anything wrong, but if you would like to see this problem fixed   "
  , "then you should report the bug at:                                              "
  , "                                                                                "
  , "https://github.com/dhall-lang/dhall-haskell/issues                              "
  ]
  where
    _ERROR :: String
    _ERROR = "\ESC[1;31mError\ESC[0m"


data Closure a = Cl !Text !(Env a) !(Expr Void a)
data VChunks a = VChunks ![(Text, Val a)] !Text

instance Semigroup (VChunks a) where
  VChunks xys z <> VChunks [] z' = VChunks xys (z <> z')
  VChunks xys z <> VChunks ((x', y'):xys') z' = VChunks (xys ++ (z <> x', y'):xys') z'

instance Monoid (VChunks a) where
  mempty = VChunks [] mempty

#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

data HLamInfo a
  = Prim
  | Typed !Text (Val a)
  | NaturalFoldCl (Val a)
  | ListFoldCl (Val a)
  | OptionalFoldCl (Val a)

pattern VPrim :: (Val a -> Val a) -> Val a
pattern VPrim f = VHLam Prim f

data Val a
  = VConst !Const
  | VVar !Text !Int
  | VPrimVar
  | VApp !(Val a) !(Val a)

  | VLam (Val a) {-# unpack #-} !(Closure a)
  | VHLam !(HLamInfo a) !(Val a -> Val a)

  | VPi  (Val a) {-# unpack #-} !(Closure a)
  | VHPi !Text (Val a) !(Val a -> Val a)

  | VBool
  | VBoolLit !Bool
  | VBoolAnd !(Val a) !(Val a)
  | VBoolOr !(Val a) !(Val a)
  | VBoolEQ !(Val a) !(Val a)
  | VBoolNE !(Val a) !(Val a)
  | VBoolIf !(Val a) !(Val a) !(Val a)

  | VNatural
  | VNaturalLit !Natural
  | VNaturalFold !(Val a) !(Val a) !(Val a) !(Val a)
  | VNaturalBuild !(Val a)
  | VNaturalIsZero !(Val a)
  | VNaturalEven !(Val a)
  | VNaturalOdd !(Val a)
  | VNaturalToInteger !(Val a)
  | VNaturalShow !(Val a)
  | VNaturalPlus !(Val a) !(Val a)
  | VNaturalTimes !(Val a) !(Val a)

  | VInteger
  | VIntegerLit !Integer
  | VIntegerShow !(Val a)
  | VIntegerToDouble !(Val a)

  | VDouble
  | VDoubleLit !Double
  | VDoubleShow !(Val a)

  | VText
  | VTextLit !(VChunks a)
  | VTextAppend !(Val a) !(Val a)
  | VTextShow !(Val a)

  | VList !(Val a)
  | VListLit !(Maybe (Val a)) !(Seq (Val a))
  | VListAppend !(Val a) !(Val a)
  | VListBuild   (Val a) !(Val a)
  | VListFold    (Val a) !(Val a) !(Val a) !(Val a) !(Val a)
  | VListLength  (Val a) !(Val a)
  | VListHead    (Val a) !(Val a)
  | VListLast    (Val a) !(Val a)
  | VListIndexed (Val a) !(Val a)
  | VListReverse (Val a) !(Val a)

  | VOptional (Val a)
  | VSome (Val a)
  | VNone (Val a)
  | VOptionalFold (Val a) !(Val a) (Val a) !(Val a) !(Val a)
  | VOptionalBuild (Val a) !(Val a)
  | VRecord !(Map Text (Val a))
  | VRecordLit !(Map Text (Val a))
  | VUnion !(Map Text (Maybe (Val a)))
  | VUnionLit !Text !(Val a) !(Map Text (Maybe (Val a)))
  | VCombine !(Val a) !(Val a)
  | VCombineTypes !(Val a) !(Val a)
  | VPrefer !(Val a) !(Val a)
  | VMerge !(Val a) !(Val a) !(Maybe (Val a))
  | VField !(Val a) !Text
  | VInject !(Map Text (Maybe (Val a))) !Text !(Maybe (Val a))
  | VProject !(Val a) !(Either (Set Text) (Val a))
  | VEmbed a

vFun :: Val a -> Val a -> Val a
vFun a b = VHPi "_" a (\_ -> b)
{-# inline vFun #-}

-- Evaluation
----------------------------------------------------------------------------------------------------

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

countName :: Text -> Env a -> Int
countName x = go (0 :: Int) where
  go !acc Empty             = acc
  go  acc (Skip env x'    ) = go (if x == x' then acc + 1 else acc) env
  go  acc (Extend env x' _) = go (if x == x' then acc + 1 else acc) env

inst :: Eq a => Closure a -> Val a -> Val a
inst (Cl x env t) !u = eval (Extend env x u) t
{-# inline inst #-}

-- Out-of-env variables have negative de Bruijn levels.
vVar :: Env a -> Var -> Val a
vVar env (V x (fromInteger -> i :: Int)) = go env i where
  go (Extend env x' v) i
    | x == x'   = if i == 0 then v else go env (i - 1)
    | otherwise = go env i
  go (Skip env x') i
    | x == x'   = if i == 0 then VVar x (countName x env) else go env (i - 1)
    | otherwise = go env i
  go Empty i = VVar x (0 - i - 1)

vApp :: Eq a => Val a -> Val a -> Val a
vApp !t !u = case t of
  VLam _ t    -> inst t u
  VHLam _ t   -> t u
  t           -> VApp t u
{-# inline vApp #-}

vCombine :: Val a -> Val a -> Val a
vCombine t u = case (t, u) of
  (VRecordLit m, u) | null m    -> u
  (t, VRecordLit m) | null m    -> t
  (VRecordLit m, VRecordLit m') -> VRecordLit (Dhall.Map.sort (Dhall.Map.unionWith vCombine m m'))
  (t, u)                        -> VCombine t u

vCombineTypes :: Val a -> Val a -> Val a
vCombineTypes t u = case (t, u) of
  (VRecord m, u) | null m -> u
  (t, VRecord m) | null m -> t
  (VRecord m, VRecord m') -> VRecord (Dhall.Map.sort (Dhall.Map.unionWith vCombineTypes m m'))
  (t, u)                  -> VCombineTypes t u

vListAppend :: Val a -> Val a -> Val a
vListAppend t u = case (t, u) of
  (VListLit _ xs, u) | null xs   -> u
  (t, VListLit _ ys) | null ys   -> t
  (VListLit t xs, VListLit _ ys) -> VListLit t (xs <> ys)
  (t, u)                         -> VListAppend t u
{-# inline vListAppend #-}

vNaturalPlus :: Val a -> Val a -> Val a
vNaturalPlus t u = case (t, u) of
  (VNaturalLit 0, u            ) -> u
  (t,             VNaturalLit 0) -> t
  (VNaturalLit m, VNaturalLit n) -> VNaturalLit (m + n)
  (t,             u            ) -> VNaturalPlus t u
{-# inline vNaturalPlus #-}

eval :: forall a. Eq a => Env a -> Expr Void a -> Val a
eval !env t =
  let
    evalE :: Expr Void a -> Val a
    evalE = eval env
    {-# inline evalE #-}

    evalChunks :: Chunks Void a -> VChunks a
    evalChunks (Chunks xys z) =
      foldr' (\(x, t) vcs ->
                case evalE t of
                  VTextLit vcs' -> VChunks [] x <> vcs' <> vcs
                  t             -> VChunks [(x, t)] mempty <> vcs)
            (VChunks [] z)
            xys
    {-# inline evalChunks #-}

  in case t of
    Const k          -> VConst k
    Var v            -> vVar env v
    Lam x a t        -> VLam (evalE a) (Cl x env t)
    Pi x a b         -> VPi (evalE a) (Cl x env b)
    App t u          -> vApp (evalE t) (evalE u)
    Let (b :| bs) t  -> go env (b:bs) where
                          go !env []     = eval env t
                          go  env (b:bs) = go (Extend env (variable b)
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
    NaturalFold      -> VPrim $ \case
                          VNaturalLit n ->
                            VHLam (Typed "natural" (VConst Type)) $ \natural ->
                            VHLam (Typed "succ" (vFun natural natural)) $ \succ ->
                            VHLam (Typed "zero" natural) $ \zero ->
                              let go !acc 0 = acc
                                  go  acc n = go (vApp succ acc) (n - 1)
                              in go zero (fromIntegral n :: Integer)
                          n ->
                            VHLam (NaturalFoldCl n) $ \natural -> VPrim $ \succ -> VPrim $ \zero ->
                              VNaturalFold n natural succ zero
    NaturalBuild     -> VPrim $ \case
                          VHLam (NaturalFoldCl x) _ -> x
                          VPrimVar -> VNaturalBuild VPrimVar
                          t        ->
                             t `vApp` VNatural
                               `vApp` VHLam (Typed "n" VNatural) (\n -> vNaturalPlus n (VNaturalLit 1))
                               `vApp` VNaturalLit 0

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
    TextAppend t u   -> evalE (TextLit (Chunks [("", t), ("", u)] ""))
    TextShow         -> VPrim $ \case
                          VTextLit (VChunks [] x) -> VTextLit (VChunks [] (textShow x))
                          t                       -> VTextShow t

    List             -> VPrim VList
    ListLit ma ts    -> VListLit (evalE <$> ma) (evalE <$> ts)
    ListAppend t u   -> vListAppend (evalE t) (evalE u)
    ListBuild        -> VPrim $ \a -> VPrim $ \case
                          VHLam (ListFoldCl x) _ -> x
                          VPrimVar -> VListBuild a VPrimVar
                          t ->
                            t `vApp` VList a
                              `vApp` VHLam (Typed "a" a) (\x ->
                                              VHLam (Typed "as" (VList a)) (\as ->
                                                vListAppend (VListLit Nothing (pure x)) as))
                              `vApp` VListLit (Just a) mempty

    ListFold         -> VPrim $ \a -> VPrim $ \case
                          VListLit _ as ->
                            VHLam (Typed "list" (VConst Type)) $ \list ->
                            VHLam (Typed "cons" (vFun a $ vFun list list) ) $ \cons ->
                            VHLam (Typed "nil"  list) $ \nil ->
                              foldr' (\x b -> cons `vApp` x `vApp` b) nil as
                          as ->
                            VHLam (ListFoldCl as) $ \t -> VPrim $ \c -> VPrim $ \n ->
                              VListFold a as t c n

    ListLength       -> VPrim $ \ a -> VPrim $ \case
                          VListLit _ as -> VNaturalLit (fromIntegral (Data.Sequence.length as))
                          as            -> VListLength a as
    ListHead         -> VPrim $ \ a -> VPrim $ \case
                          VListLit _ as -> case Data.Sequence.viewl as of
                                             y Data.Sequence.:< _ -> VSome y
                                             _                    -> VNone a
                          as            -> VListHead a as
    ListLast         -> VPrim $ \ a -> VPrim $ \case
                          VListLit _ as -> case Data.Sequence.viewr as of
                                             _ Data.Sequence.:> t -> VSome t
                                             _                    -> VNone a
                          as            -> VListLast a as
    ListIndexed      -> VPrim $ \ a -> VPrim $ \case
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

    Optional         -> VPrim VOptional
    Some t           -> VSome (evalE t)
    None             -> VPrim $ \ ~a -> VNone a

    OptionalFold     -> VPrim $ \ ~a -> VPrim $ \case
                          VNone _ ->
                            VHLam (Typed "optional" (VConst Type)) $ \optional ->
                            VHLam (Typed "some" (vFun a optional)) $ \some ->
                            VHLam (Typed "none" optional) $ \none ->
                            none
                          VSome t ->
                            VHLam (Typed "optional" (VConst Type)) $ \optional ->
                            VHLam (Typed "some" (vFun a optional)) $ \some ->
                            VHLam (Typed "none" optional) $ \none ->
                            some `vApp` t
                          opt ->
                            VHLam (OptionalFoldCl opt) $ \o ->
                            VPrim $ \s ->
                            VPrim $ \n ->
                            VOptionalFold a opt o s n
    OptionalBuild    -> VPrim $ \ ~a -> VPrim $ \case
                          VHLam (OptionalFoldCl x) _ -> x
                          VPrimVar -> VOptionalBuild a VPrimVar
                          t -> t `vApp` VOptional a
                                 `vApp` VHLam (Typed "a" a) VSome
                                 `vApp` VNone a

    Record kts       -> VRecord (Dhall.Map.sort (evalE <$> kts))
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
                            | otherwise -> error errorMsg
                          (VRecordLit m, VInject _ k mt, _)
                            | Just f  <- Dhall.Map.lookup k m -> maybe f (vApp f) mt
                            | otherwise -> error errorMsg
                          (x, y, ma) -> VMerge x y ma
    Field t k        -> case evalE t of
                          VRecordLit m
                            | Just v <- Dhall.Map.lookup k m -> v
                            | otherwise -> error errorMsg
                          VUnion m -> case Dhall.Map.lookup k m of
                            Just (Just _) -> VPrim $ \ ~u -> VInject m k (Just u)
                            Just Nothing  -> VInject m k Nothing
                            _             -> error errorMsg
                          t -> VField t k
    Project t (Left ks) ->
                        if null ks then
                          VRecordLit mempty
                        else case evalE t of
                          VRecordLit kvs
                            | Just s <- traverse (\k -> (k,) <$> Dhall.Map.lookup k kvs) (toList ks)
                              -> VRecordLit (Dhall.Map.sort (Dhall.Map.fromList s))
                            | otherwise -> error errorMsg
                          t -> VProject t (Left ks)
    Project t (Right e) ->
                        case evalE e of
                          VRecord kts ->
                            evalE (Project t (Left (Dhall.Set.fromList (Dhall.Map.keys kts))))
                          e' -> VProject (evalE t) (Right e')
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

eqMapsBy :: Ord k => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
eqMapsBy f mL mR = eqListBy eq (Dhall.Map.toList mL) (Dhall.Map.toList mR)
  where
    eq (kL, vL) (kR, vR) = kL == kR && f vL vR
{-# inline eqMapsBy #-}

eqMaybeBy :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybeBy f = go where
  go (Just x) (Just y) = f x y
  go Nothing  Nothing  = True
  go _        _        = False
{-# inline eqMaybeBy #-}

conv :: forall a. Eq a => Env a -> Val a -> Val a -> Bool
conv !env t t' =
  let
    fresh :: Text -> (Text, Val a)
    fresh x = (x, VVar x (countName x env))
    {-# inline fresh #-}

    freshCl :: Closure a -> (Text, Val a, Closure a)
    freshCl cl@(Cl x _ _) = (x, snd (fresh x), cl)
    {-# inline freshCl #-}

    convChunks :: VChunks a -> VChunks a -> Bool
    convChunks (VChunks xys z) (VChunks xys' z') =
      eqListBy (\(x, y) (x', y') -> x == x' && conv env y y') xys xys' && z == z'
    {-# inline convChunks #-}

    convE :: Val a -> Val a -> Bool
    convE = conv env
    {-# inline convE #-}

    convSkip :: Text -> Val a -> Val a -> Bool
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

    (VApp t u, VApp t' u') -> convE t t' && convE u u'

    (VPi a b, VPi a' (freshCl -> (x, v, b'))) ->
      convE a a' && convSkip x (inst b v) (inst b' v)
    (VPi a b, VHPi (fresh -> (x, v)) a' b') ->
      convE a a' && convSkip x (inst b v) (b' v)
    (VHPi _ a b, VPi a' (freshCl -> (x, v, b'))) ->
      convE a a' && convSkip x (b v) (inst b' v)
    (VHPi _ a b, VHPi (fresh -> (x, v)) a' b') ->
      convE a a' && convSkip x (b v) (b' v)

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
    (VDoubleLit n  , VDoubleLit n')  -> Dhall.Binary.encode (DoubleLit n  :: Expr Void Import) ==
                                        Dhall.Binary.encode (DoubleLit n' :: Expr Void Import)
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
    (VRecord m               , VRecord m'                  ) -> eqMapsBy convE m m'
    (VRecordLit m            , VRecordLit m'               ) -> eqMapsBy convE m m'
    (VUnion m                , VUnion m'                   ) -> eqMapsBy (eqMaybeBy convE) m m'
    (VUnionLit k v m         , VUnionLit k' v' m'          ) -> k == k' && convE v v' &&
                                                                  eqMapsBy (eqMaybeBy convE)  m m'
    (VCombine t u            , VCombine t' u'              ) -> convE t t' && convE u u'
    (VCombineTypes t u       , VCombineTypes t' u'         ) -> convE t t' && convE u u'
    (VPrefer  t u            , VPrefer t' u'               ) -> convE t t' && convE u u'
    (VMerge t u _            , VMerge t' u' _              ) -> convE t t' && convE u u'
    (VField t k              , VField t' k'                ) -> convE t t' && k == k'
    (VProject t (Left ks)    , VProject t' (Left ks')      ) -> convE t t' && ks == ks'
    (VProject t (Right e)    , VProject t' (Right e')      ) -> convE t t' && convE e e'
    (VInject m k mt          , VInject m' k' mt'           ) -> eqMapsBy (eqMaybeBy convE) m m'
                                                                  && k == k' && eqMaybeBy convE mt mt'
    (VEmbed a                , VEmbed a'                   ) -> a == a'
    (VOptionalFold a t _ u v , VOptionalFold a' t' _ u' v' ) ->
      convE a a' && convE t t' && convE u u' && convE v v'

    (_, _) -> False

convEmpty :: Eq a => Expr s a -> Expr t a -> Bool
convEmpty (denote -> t) (denote -> u) = conv Empty (eval Empty t) (eval Empty u)

-- Quoting
----------------------------------------------------------------------------------------------------

data Names
  = NEmpty
  | NBind !Names {-# unpack #-} !Text
  deriving Show

envNames :: Env a -> Names
envNames Empty = NEmpty
envNames (Skip   env x  ) = NBind (envNames env) x
envNames (Extend env x _) = NBind (envNames env) x

countName' :: Text -> Names -> Int
countName' x = go 0 where
  go !acc NEmpty         = acc
  go  acc (NBind env x') = go (if x == x' then acc + 1 else acc) env

-- | Quote a value into beta-normal form.
quote :: forall a. Eq a => Names -> Val a -> Expr Void a
quote !env !t =
  let
    fresh :: Text -> (Text, Val a)
    fresh x = (x, VVar x (countName' x env))
    {-# inline fresh #-}

    freshCl :: Closure a -> (Text, Val a, Closure a)
    freshCl cl@(Cl x _ _) = (x, snd (fresh x), cl)
    {-# inline freshCl #-}

    qVar :: Text -> Int -> Expr Void a
    qVar !x !i = Var (V x (fromIntegral (countName' x env - i - 1)))
    {-# inline qVar #-}

    quoteE :: Val a -> Expr Void a
    quoteE = quote env
    {-# inline quoteE #-}

    quoteBind :: Text -> Val a -> Expr Void a
    quoteBind x = quote (NBind env x)
    {-# inline quoteBind #-}

    qApp :: Expr Void a -> Val a -> Expr Void a
    qApp t VPrimVar = t
    qApp t u        = App t (quoteE u)
    {-# inline qApp #-}

  in case t of
    VConst k                      -> Const k
    VVar x i                      -> qVar x i
    VApp t u                      -> quoteE t `qApp` u
    VLam a (freshCl -> (x, v, t)) -> Lam x (quoteE a) (quoteBind x (inst t v))
    VHLam i t                     -> case i of
                                       Typed (fresh -> (x, v)) a -> Lam x (quoteE a) (quoteBind x (t v))
                                       Prim                      -> quote env (t VPrimVar)
                                       NaturalFoldCl{}           -> quote env (t VPrimVar)
                                       ListFoldCl{}              -> quote env (t VPrimVar)
                                       OptionalFoldCl{}          -> quote env (t VPrimVar)

    VPi a (freshCl -> (x, v, b))  -> Pi x (quoteE a) (quoteBind x (inst b v))
    VHPi (fresh -> (x, v)) a b    -> Pi x (quoteE a) (quoteBind x (b v))

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
    VProject t p                  -> Project (quoteE t) (fmap quoteE p)
    VInject m k Nothing           -> Field (Union ((quoteE <$>) <$> m)) k
    VInject m k (Just t)          -> Field (Union ((quoteE <$>) <$> m)) k `qApp` t
    VEmbed a                      -> Embed a
    VPrimVar                      -> error errorMsg

-- Normalization
----------------------------------------------------------------------------------------------------

-- | Normalize an expression in an environment of values. Any variable pointing out of
--   the environment is treated as opaque free variable.
nf :: Eq a => Env a -> Expr s a -> Expr t a
nf !env = coeExprVoid . quote (envNames env) . eval env . denote

-- | Normalize an expression in an empty environment.
nfEmpty :: Eq a => Expr s a -> Expr t a
nfEmpty = nf Empty

-- Alpha-renaming
--------------------------------------------------------------------------------

-- | Rename all binders to "_".
alphaNormalize :: Expr s a -> Expr s a
alphaNormalize = goEnv NEmpty where

  goVar :: Names -> Text -> Integer -> Expr s a
  goVar e topX topI = go 0 e topI where
    go !acc (NBind env x) !i
      | x == topX = if i == 0 then Var (V "_" acc) else go (acc + 1) env (i - 1)
      | otherwise = go (acc + 1) env i
    go acc NEmpty i = Var (V topX i)

  goEnv :: Names -> Expr s a -> Expr s a
  goEnv !e t = let

    go                     = goEnv e
    goBind x               = goEnv (NBind e x)
    goChunks (Chunks ts x) = Chunks ((go <$>) <$> ts) x

    in case t of
      Const k          -> Const k
      Var (V x i)      -> goVar e x i
      Lam x t u        -> Lam "_" (go t) (goBind x u)
      Pi x a b         -> Pi "_" (go a) (goBind x b)
      App t u          -> App (go t) (go u)

      Let (b :| bs) u  ->
        let Binding x a t = b

            nil = (NBind e x, Binding "_" (goEnv e <$> a) (goEnv e t) :| [])

            snoc (e, bs) (Binding x a t) =
                (NBind e x, cons (Binding "_" (goEnv e <$> a) (goEnv e t)) bs)

            (e', Data.List.NonEmpty.reverse -> bs') = foldl' snoc nil bs

        in Let bs' (goEnv e' u)

      Annot t u        -> Annot (go t) (go u)
      Bool             -> Bool
      BoolLit b        -> BoolLit b
      BoolAnd t u      -> BoolAnd (go t) (go u)
      BoolOr t u       -> BoolOr  (go t) (go u)
      BoolEQ t u       -> BoolEQ  (go t) (go u)
      BoolNE t u       -> BoolNE  (go t) (go u)
      BoolIf b t f     -> BoolIf  (go b) (go t) (go f)
      Natural          -> Natural
      NaturalLit n     -> NaturalLit n
      NaturalFold      -> NaturalFold
      NaturalBuild     -> NaturalBuild
      NaturalIsZero    -> NaturalIsZero
      NaturalEven      -> NaturalEven
      NaturalOdd       -> NaturalOdd
      NaturalToInteger -> NaturalToInteger
      NaturalShow      -> NaturalShow
      NaturalPlus t u  -> NaturalPlus  (go t) (go u)
      NaturalTimes t u -> NaturalTimes (go t) (go u)
      Integer          -> Integer
      IntegerLit n     -> IntegerLit n
      IntegerShow      -> IntegerShow
      IntegerToDouble  -> IntegerToDouble
      Double           -> Double
      DoubleLit n      -> DoubleLit n
      DoubleShow       -> DoubleShow
      Text             -> Text
      TextLit cs       -> TextLit (goChunks cs)
      TextAppend t u   -> TextAppend (go t) (go u)
      TextShow         -> TextShow
      List             -> List
      ListLit ma ts    -> ListLit (go <$> ma) (go <$> ts)
      ListAppend t u   -> ListAppend (go t) (go u)
      ListBuild        -> ListBuild
      ListFold         -> ListFold
      ListLength       -> ListLength
      ListHead         -> ListHead
      ListLast         -> ListLast
      ListIndexed      -> ListIndexed
      ListReverse      -> ListReverse
      Optional         -> Optional
      Some t           -> Some (go t)
      None             -> None
      OptionalFold     -> OptionalFold
      OptionalBuild    -> OptionalBuild
      Record kts       -> Record (go <$> kts)
      RecordLit kts    -> RecordLit (go <$> kts)
      Union kts        -> Union ((go <$>) <$> kts)
      UnionLit k v kts -> UnionLit k (go v) ((go <$>) <$> kts)
      Combine t u      -> Combine (go t) (go u)
      CombineTypes t u -> CombineTypes  (go t) (go u)
      Prefer t u       -> Prefer (go t) (go u)
      Merge x y ma     -> Merge (go x) (go y) (go <$> ma)
      Field t k        -> Field (go t) k
      Project t ks     -> Project (go t) (go <$> ks)
      Note s e         -> Note s (go e)
      ImportAlt t u    -> ImportAlt (go t) (go u)
      Embed a          -> Embed a
