{-# LANGUAGE TupleSections #-}
{-# LANGUAGE
  BangPatterns,
  CPP,
  LambdaCase,
  OverloadedStrings,
  PatternSynonyms,
  ScopedTypeVariables,
  ViewPatterns,
  MagicHash
  #-}

{-# OPTIONS_GHC
  -O
  -Wall
  -fno-warn-name-shadowing
  #-}

{-|
Eval-apply environment machine with conversion checking and quoting to normal
forms. Fairly similar to GHCI's STG machine algorithmically, but much simpler,
with no optimization for known calls or environment trimming.
-}

module Dhall.Eval (
    inst
  , conv
  , convEmpty
  , eval
  , evalEmpty
  -- , freeIn
  , nf
  , nfEmpty
  , pattern VAnyPi
  , quote
  , vCombineTypes
  , QuoteOption(..)
  ) where


#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif

import Data.Foldable (foldr', toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)

import Dhall.Core (
    Expr(..)
  , Binding(..)
  , Chunks(..)
  , Closure(..)
  , Const(..)
  , Env(..)
  , HLamInfo(..)
  , I(..)
  , VChunks(..)
  , Val(..)
  , X
  , internalError
  , pattern VPrim
  , vFun
  , Projection(..)
  , VProjection(..)
  , Injection(..)
  , AnnotSource(..)
  )

import Dhall.Map (Map)
import GHC.Prim (reallyUnsafePtrEquality#)

import qualified Data.Char
import qualified Data.Sequence
import qualified Data.Text
import qualified Dhall.Binary
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Text.Printf

-- Evaluation
----------------------------------------------------------------------------------------------------

evalError :: String
evalError = internalError "An ill-typed expression was encountered during normalization."

ptrEq :: a -> a -> Bool
ptrEq !a !a' = case reallyUnsafePtrEquality# a a' of
  1# -> True
  _  -> False
{-# INLINE ptrEq #-}

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

inst :: Closure -> Val -> Val
inst (Cl _ env t) !u = eval (Extend env u) t
{-# INLINE inst #-}

envLength :: Env -> Int
envLength = go 0 where
  go !acc Empty          = acc
  go  acc (Extend env _) = go (acc + 1) env
  go  acc (Skip env)     = go acc env

vVar :: Env -> Int -> Val
vVar = go where
  go (Extend _ v)   0 = v
  go (Skip env)     0 = VVar (envLength env)
  go (Extend env _) i = go env (i - 1)
  go (Skip env)     i = go env (i - 1)
  go Empty          _ = error evalError

-- | Pattern synonym for matching any Pi value.
pattern VAnyPi :: Text -> Val -> Const -> Const -> (Val -> Val) -> Val
pattern VAnyPi x a i j b <- ((\case VPi i j a b@(Cl x _ _) -> Just (x, i, j, a, inst b)
                                    VHPi x i j a b         -> Just (x, i, j, a, b)
                                    _                      -> Nothing)
                         -> Just (x, i, j, a, b))

vApp :: Val -> Val -> Val
vApp !t !u = case t of
  VLam _ t    -> inst t u
  VHLam _ t   -> t u
  t           -> VApp t u
{-# INLINE vApp #-}

vCombine :: Val -> Val -> Val
vCombine t u = case (t, u) of
  (VRecordLit m, u) | null m    -> u
  (t, VRecordLit m) | null m    -> t
  (VRecordLit m, VRecordLit m') -> VRecordLit (Dhall.Map.sort (Dhall.Map.unionWith vCombine m m'))
  (t, u)                        -> VCombine t u

vCombineTypes :: Val -> Val -> Val
vCombineTypes t u = case (t, u) of
  (VRecord m, u) | null m -> u
  (t, VRecord m) | null m -> t
  (VRecord m, VRecord m') -> VRecord (Dhall.Map.sort (Dhall.Map.unionWith vCombineTypes m m'))
  (t, u)                  -> VCombineTypes t u

vListAppend :: Val -> Val -> Val
vListAppend t u = case (t, u) of
  (VListLit _ xs, u) | null xs   -> u
  (t, VListLit _ ys) | null ys   -> t
  (VListLit t xs, VListLit _ ys) -> VListLit t (xs <> ys)
  (t, u)                         -> VListAppend t u
{-# INLINE vListAppend #-}

vNaturalPlus :: Val -> Val -> Val
vNaturalPlus t u = case (t, u) of
  (VNaturalLit 0, u            ) -> u
  (t,             VNaturalLit 0) -> t
  (VNaturalLit m, VNaturalLit n) -> VNaturalLit (m + n)
  (t,             u            ) -> VNaturalPlus t u
{-# INLINE vNaturalPlus #-}

evalEmpty :: Expr I -> Val
evalEmpty = eval Empty
{-# INLINE evalEmpty #-}

eval :: Env -> Expr I -> Val
eval !env t =
  let
    evalE :: Expr I -> Val
    evalE = eval env
    {-# INLINE evalE #-}

    evalChunks :: Chunks (Expr I) -> VChunks
    evalChunks (Chunks xys z) =
      foldr' (\(x, t) vcs ->
                case evalE t of
                  VTextLit vcs' -> VChunks [] x <> vcs' <> vcs
                  t             -> VChunks [(x, t)] mempty <> vcs)
            (VChunks [] z)
            xys
    {-# INLINE evalChunks #-}

  in case t of
    Const k          -> VConst k
    Var i            -> vVar env i
    Lam (x, _) a t   -> VLam (evalE a) (Cl x env t)
    Pi x i j a b     -> VPi i j (evalE a) (Cl x env b)
    App t u          -> vApp (evalE t) (evalE u)
    Let (b :| bs) t  -> go env (b:bs) where
                          go !env []     = eval env t
                          go  env (b:bs) = go (Extend env (eval env (value b))) bs
    Annot t _        -> evalE t

    Bool             -> VBool
    BoolLit b        -> VBoolLit b
    BoolAnd t u      -> case (evalE t, evalE u) of
                          (VBoolLit True, u)                -> u
                          (VBoolLit False, _)               -> VBoolLit False
                          (t, VBoolLit True)                -> t
                          (_, VBoolLit False)               -> VBoolLit False
                          (t, u) | conv (envLength env) t u -> t
                          (t, u)                            -> VBoolAnd t u
    BoolOr t u       -> case (evalE t, evalE u) of
                          (VBoolLit False, u)               -> u
                          (VBoolLit True, _)                -> VBoolLit True
                          (t, VBoolLit False)               -> t
                          (_, VBoolLit True)                -> VBoolLit True
                          (t, u) | conv (envLength env) t u -> t
                          (t, u)                            -> VBoolOr t u
    BoolEQ t u       -> case (evalE t, evalE u) of
                          (VBoolLit True, u)                -> u
                          (t, VBoolLit True)                -> t
                          (t, u) | conv (envLength env) t u -> VBoolLit True
                          (t, u)                            -> VBoolEQ t u
    BoolNE t u       -> case (evalE t, evalE u) of
                          (VBoolLit False, u)               -> u
                          (t, VBoolLit False)               -> t
                          (t, u) | conv (envLength env) t u -> VBoolLit False
                          (t, u)                            -> VBoolNE t u
    BoolIf b t f     -> case (evalE b, evalE t, evalE f) of
                          (VBoolLit True,  t, _)               -> t
                          (VBoolLit False, _, f)               -> f
                          (b, VBoolLit True, VBoolLit False)   -> b
                          (_, t, f) | conv (envLength env) t f -> t
                          (b, t, f)                            -> VBoolIf b t f

    Natural          -> VNatural
    NaturalLit n     -> VNaturalLit n
    NaturalFold      -> VPrim $ \case
                          VNaturalLit n ->
                            VHLam (Typed "natural" (VConst Type)) $ \natural ->
                            VHLam (Typed "succ" (vFun Type Type natural natural)) $ \succ ->
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
                          (VNaturalLit 0, _            ) -> VNaturalLit 0
                          (_,             VNaturalLit 0) -> VNaturalLit 0
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
    ListLit ma ts    -> VListLit (evalE . fst <$> ma) (evalE <$> ts)
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
                            VHLam (Typed "cons" (vFun Type Type a $ vFun Type Type list list)) $ \cons ->
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
                                   Just (VList (VRecord (Dhall.Map.fromList
                                                         [("index", VNatural), ("value", a)])))
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
                          VListLit t as | null as -> VListLit t mempty
                          VListLit _ as -> VListLit Nothing (Data.Sequence.reverse as)
                          t             -> VListReverse a t
    Optional         -> VPrim VOptional
    Some t           -> VSome (evalE t)
    None             -> VPrim $ \ ~a -> VNone a

    OptionalFold     -> VPrim $ \ ~a -> VPrim $ \case
                          VNone _ ->
                            VHLam (Typed "optional" (VConst Type)) $ \optional ->
                            VHLam (Typed "some" (vFun Type Type a optional)) $ \_ ->
                            VHLam (Typed "none" optional) $ \none ->
                            none
                          VSome t ->
                            VHLam (Typed "optional" (VConst Type)) $ \optional ->
                            VHLam (Typed "some" (vFun Type Type a optional)) $ \some ->
                            VHLam (Typed "none" optional) $ \_ ->
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
    Combine t u      -> vCombine (evalE t) (evalE u)
    CombineTypes t u -> vCombineTypes (evalE t) (evalE u)
    Prefer t u       -> case (evalE t, evalE u) of
                          (VRecordLit m, u) | null m -> u
                          (t, VRecordLit m) | null m -> t
                          (VRecordLit m, VRecordLit m') ->
                             VRecordLit (Dhall.Map.sort (Dhall.Map.union m' m))
                          (t, u) -> VPrefer t u
    Merge x y ma     -> case (evalE x, evalE y, evalE . fst <$> ma) of
                          (VRecordLit m, VInject _ k mt, _)
                            | Just f  <- Dhall.Map.lookup k m -> maybe f (vApp f) mt
                            | otherwise -> error evalError
                          (x, y, ma) -> VMerge x y ma
    ToMap x ma       -> case (evalE x, evalE . fst <$> ma) of
                          (VRecordLit m, ma'@(Just _)) | null m ->
                            VListLit ma' (Data.Sequence.empty)
                          (VRecordLit m, _) -> let
                            entry (k, v) =
                              VRecordLit (Dhall.Map.fromList [("mapKey", VTextLit $ VChunks [] k),
                                                              ("mapValue", v)])
                            s = (Data.Sequence.fromList . map entry . Dhall.Map.toList) m
                            in VListLit Nothing s
                          (x, ma) -> VToMap x ma
    Project t p      -> case p of
                          ProjSingle k -> case evalE t of
                            VRecordLit m
                              | Just v <- Dhall.Map.lookup k m -> v
                              | otherwise -> error evalError
                            t -> VProject t (VProjSingle k)
                          ProjSet ks me -> case evalE t of
                            VRecordLit kvs -> let
                               kvs' = Dhall.Map.restrictKeys kvs (Dhall.Set.toSet ks)
                               in VRecordLit (Dhall.Map.sort kvs')
                            t -> VProject t (VProjSet (Dhall.Set.sort ks) (evalE <$> me))
    Inject t k i     -> case i of
                          InjEnum  -> VPrim $ \ u -> VInject (evalE t) k (Just u)
                          InjField -> VInject (evalE t) k Nothing
    EmbedImport i    -> case i of I _ _ v -> v
    ImportAlt l r b  -> if b then evalE l else evalE r


-- Conversion checking
--------------------------------------------------------------------------------

eqListBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqListBy f = go where
  go (x:xs) (y:ys) | f x y = go xs ys
  go [] [] = True
  go _  _  = False
{-# INLINE eqListBy #-}

eqMapsBy :: Ord k => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
eqMapsBy f mL mR = eqListBy eq (Dhall.Map.toList mL) (Dhall.Map.toList mR)
  where
    eq (kL, vL) (kR, vR) = kL == kR && f vL vR
{-# INLINE eqMapsBy #-}

eqMaybeBy :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybeBy f = go where
  go (Just x) (Just y) = f x y
  go Nothing  Nothing  = True
  go _        _        = False
{-# INLINE eqMaybeBy #-}

conv :: Int -> Val -> Val -> Bool
conv !lvl t t' =
  let
    fresh :: Val
    fresh = VVar lvl
    {-# INLINE fresh #-}

    convChunks :: VChunks -> VChunks -> Bool
    convChunks (VChunks xys z) (VChunks xys' z') =
      eqListBy (\(x, y) (x', y') -> x == x' && conv lvl y y') xys xys' && z == z'
    {-# INLINE convChunks #-}

    convL :: Val -> Val -> Bool
    convL = conv lvl
    {-# INLINE convL #-}

    convSkip :: Val -> Val -> Bool
    convSkip = conv (lvl + 1)

  in case (t, t') of
    _ | ptrEq t t' -> True
    (VConst k, VConst k') -> k == k'
    (VVar x,   VVar x'  ) -> x == x'

    (VLam _ t,  VLam _ t' ) -> convSkip (inst t fresh) (inst t' fresh)
    (VLam _ t,  VHLam _ t') -> convSkip (inst t fresh) (t' fresh)
    (VLam _ t , t'        ) -> convSkip (inst t fresh) (vApp t' fresh)
    (VHLam _ t, VLam _ t' ) -> convSkip (t fresh) (inst t' fresh)
    (VHLam _ t, VHLam _ t') -> convSkip (t fresh) (t' fresh)
    (VHLam _ t, t'        ) -> convSkip (t fresh) (vApp t' fresh)

    (t, VLam _ t' ) -> convSkip (vApp t fresh) (inst t' fresh)
    (t, VHLam _ t') -> convSkip (vApp t fresh) (t' fresh)

    (VApp t u, VApp t' u') -> convL t t' && convL u u'

    (VPi i j a b, VPi i' j' a' b') ->
      i == i' && j == j' && convL a a' && convSkip (inst b fresh) (inst b' fresh)
    (VPi i j a b, VHPi _ i' j' a' b') ->
      i == i' && j == j' && convL a a' && convSkip (inst b fresh) (b' fresh)
    (VHPi _ i j a b, VPi i' j' a' b') ->
      i == i' && j == j' && convL a a' && convSkip (b fresh) (inst b' fresh)
    (VHPi _ i j a b, VHPi _ i' j' a' b') ->
      i == i' && j == j' && convL a a' && convSkip (b fresh) (b' fresh)

    (VBool       , VBool            ) -> True
    (VBoolLit b  , VBoolLit b'      ) -> b == b'
    (VBoolAnd t u, VBoolAnd t' u'   ) -> convL t t' && convL u u'
    (VBoolOr  t u, VBoolOr  t' u'   ) -> convL t t' && convL u u'
    (VBoolEQ  t u, VBoolEQ  t' u'   ) -> convL t t' && convL u u'
    (VBoolNE  t u, VBoolNE  t' u'   ) -> convL t t' && convL u u'
    (VBoolIf t u v, VBoolIf t' u' v') -> convL t t' && convL u u' && convL v v'

    (VNatural, VNatural) -> True
    (VNaturalLit n, VNaturalLit n') -> n == n'
    (VNaturalFold t _ u v, VNaturalFold t' _ u' v') ->
      convL t t' && convL u u' && convL v v'

    (VNaturalBuild t     , VNaturalBuild t')     -> convL t t'
    (VNaturalIsZero t    , VNaturalIsZero t')    -> convL t t'
    (VNaturalEven t      , VNaturalEven t')      -> convL t t'
    (VNaturalOdd t       , VNaturalOdd t')       -> convL t t'
    (VNaturalToInteger t , VNaturalToInteger t') -> convL t t'
    (VNaturalShow t      , VNaturalShow t')      -> convL t t'
    (VNaturalPlus t u    , VNaturalPlus t' u')   -> convL t t' && convL u u'
    (VNaturalTimes t u   , VNaturalTimes t' u')  -> convL t t' && convL u u'

    (VInteger           , VInteger)            -> True
    (VIntegerLit t      , VIntegerLit t')      -> t == t'
    (VIntegerShow t     , VIntegerShow t')     -> convL t t'
    (VIntegerToDouble t , VIntegerToDouble t') -> convL t t'

    (VDouble       , VDouble)        -> True
    (VDoubleLit n  , VDoubleLit n')  -> Dhall.Binary.encode (DoubleLit n  :: Expr X) ==
                                        Dhall.Binary.encode (DoubleLit n' :: Expr X)
    (VDoubleShow t , VDoubleShow t') -> convL t t'

    (VText, VText) -> True

    (VTextLit cs     , VTextLit cs')      -> convChunks cs cs'
    (VTextAppend t u , VTextAppend t' u') -> convL t t' && convL u u'
    (VTextShow t     , VTextShow t')      -> convL t t'

    (VList a        , VList a'      ) -> convL a a'
    (VListLit _ xs  , VListLit _ xs') -> eqListBy convL (toList xs) (toList xs')

    (VListAppend t u     , VListAppend t' u'       ) -> convL t t' && convL u u'
    (VListBuild _ t      , VListBuild _ t'         ) -> convL t t'
    (VListLength a t     , VListLength a' t'       ) -> convL a a' && convL t t'
    (VListHead _ t       , VListHead _ t'          ) -> convL t t'
    (VListLast _ t       , VListLast _ t'          ) -> convL t t'
    (VListIndexed _ t    , VListIndexed _ t'       ) -> convL t t'
    (VListReverse _ t    , VListReverse _ t'       ) -> convL t t'
    (VListFold a l _ t u , VListFold a' l' _ t' u' ) ->
      convL a a' && convL l l' && convL t t' && convL u u'

    (VOptional a               , VOptional a'                ) -> convL a a'
    (VSome t                   , VSome t'                    ) -> convL t t'
    (VNone _                   , VNone _                     ) -> True
    (VOptionalBuild _ t        , VOptionalBuild _ t'         ) -> convL t t'
    (VRecord m                 , VRecord m'                  ) -> eqMapsBy convL m m'
    (VRecordLit m              , VRecordLit m'               ) -> eqMapsBy convL m m'
    (VUnion m                  , VUnion m'                   ) -> eqMapsBy (eqMaybeBy convL) m m'
    (VCombine t u              , VCombine t' u'              ) -> convL t t' && convL u u'
    (VCombineTypes t u         , VCombineTypes t' u'         ) -> convL t t' && convL u u'
    (VPrefer  t u              , VPrefer t' u'               ) -> convL t t' && convL u u'
    (VMerge t u _              , VMerge t' u' _              ) -> convL t t' && convL u u'
    (VToMap t _                , VToMap t' _                 ) -> convL t t'
    (VProject t (VProjSingle k), VProject t' (VProjSingle k')) -> convL t t' && k == k'
    (VProject t (VProjSet ks _), VProject t' (VProjSet ks' _)) -> convL t t' && ks == ks'
    (VInject t k Nothing       , VInject t' k' Nothing       ) -> convL t t' && k == k'
    (VInject t k (Just u)      , VInject t' k' (Just u')     ) -> convL t t' && k == k' && convL u u'

    (VOptionalFold a t _ u v , VOptionalFold a' t' _ u' v' ) ->
      convL a a' && convL t t' && convL u u' && convL v v'

    (_, _) -> False

convEmpty :: Expr I -> Expr I -> Bool
convEmpty t u = conv 0 (eval Empty t) (eval Empty u)

-- Quoting
----------------------------------------------------------------------------------------------------

data QuoteOption = Alpha | NoAlpha

-- | Quote a value into beta-normal form.
quote :: QuoteOption -> Int -> Val -> Expr X
quote !opt !lvl !t =
  let
    fresh :: Text -> (Text, Val)
    fresh x = case opt of
      Alpha   -> ("_", VVar lvl)
      NoAlpha -> (x  , VVar lvl)
    {-# INLINE fresh #-}

    freshCl :: Closure -> (Text, Val, Closure)
    freshCl cl@(Cl x _ _) = (x, snd (fresh x), cl)
    {-# INLINE freshCl #-}

    qVar :: Int -> Expr X
    qVar i = Var (lvl - i - 1)
    {-# INLINE qVar #-}

    quoteL :: Val -> Expr X
    quoteL = quote opt lvl
    {-# INLINE quoteL #-}

    quoteBind :: Val -> Expr X
    quoteBind = quote opt (lvl + 1)
    {-# INLINE quoteBind #-}

    qApp :: Expr X -> Val -> Expr X
    qApp t VPrimVar = t
    qApp t u        = App t (quoteL u)
    {-# INLINE qApp #-}

  in case t of
    VConst k                      -> Const k
    VVar i                        -> qVar i
    VApp t u                      -> quoteL t `qApp` u
    VLam a (freshCl -> (x, v, t)) -> Lam (x, ElabAnnot) (quoteL a) (quoteBind (inst t v))
    VHLam i t                     -> case i of
                                       Typed (fresh -> (x, v)) a -> Lam (x, ElabAnnot) (quoteL a) (quoteBind (t v))
                                       Prim                      -> quoteL (t VPrimVar)
                                       NaturalFoldCl{}           -> quoteL (t VPrimVar)
                                       ListFoldCl{}              -> quoteL (t VPrimVar)
                                       OptionalFoldCl{}          -> quoteL (t VPrimVar)

    VPi i j a (freshCl -> (x, v, b)) -> Pi x i j (quoteL a) (quoteBind (inst b v))
    VHPi (fresh -> (x, v)) i j a b   -> Pi x i j (quoteL a) (quoteBind (b v))

    VBool                   -> Bool
    VBoolLit b              -> BoolLit b
    VBoolAnd t u            -> BoolAnd (quoteL t) (quoteL u)
    VBoolOr t u             -> BoolOr (quoteL t) (quoteL u)
    VBoolEQ t u             -> BoolEQ (quoteL t) (quoteL u)
    VBoolNE t u             -> BoolNE (quoteL t) (quoteL u)
    VBoolIf t u v           -> BoolIf (quoteL t) (quoteL u) (quoteL v)

    VNatural                -> Natural
    VNaturalLit n           -> NaturalLit n
    VNaturalFold a t u v    -> NaturalFold `qApp` a `qApp` t `qApp` u `qApp` v
    VNaturalBuild t         -> NaturalBuild `qApp` t
    VNaturalIsZero t        -> NaturalIsZero `qApp` t
    VNaturalEven t          -> NaturalEven `qApp` t
    VNaturalOdd t           -> NaturalOdd `qApp` t
    VNaturalToInteger t     -> NaturalToInteger `qApp` t
    VNaturalShow t          -> NaturalShow `qApp` t
    VNaturalPlus t u        -> NaturalPlus (quoteL t) (quoteL u)
    VNaturalTimes t u       -> NaturalTimes (quoteL t) (quoteL u)

    VInteger                -> Integer
    VIntegerLit n           -> IntegerLit n
    VIntegerShow t          -> IntegerShow `qApp` t
    VIntegerToDouble t      -> IntegerToDouble `qApp` t

    VDouble                 -> Double
    VDoubleLit n            -> DoubleLit n
    VDoubleShow t           -> DoubleShow `qApp` t

    VText                   -> Text
    VTextLit (VChunks xys z)-> TextLit (Chunks ((quoteL <$>) <$> xys) z)
    VTextAppend t u         -> TextAppend (quoteL t) (quoteL u)
    VTextShow t             -> TextShow `qApp` t

    VList t                 -> List `qApp` t
    VListLit ma ts          -> ListLit ((,ElabAnnot) . quoteL <$> ma) (quoteL <$> ts)
    VListAppend t u         -> ListAppend (quoteL t) (quoteL u)
    VListBuild a t          -> ListBuild `qApp` a `qApp` t
    VListFold a l t u v     -> ListFold `qApp` a `qApp` l `qApp` t `qApp` u `qApp` v
    VListLength a t         -> ListLength `qApp` a `qApp` t
    VListHead a t           -> ListHead `qApp` a `qApp` t
    VListLast a t           -> ListLast `qApp` a `qApp` t
    VListIndexed a t        -> ListIndexed `qApp` a `qApp` t
    VListReverse a t        -> ListReverse `qApp` a `qApp` t

    VOptional a             -> Optional `qApp` a
    VSome t                 -> Some (quoteL t)
    VNone t                 -> None `qApp` t
    VOptionalFold a o t u v -> OptionalFold `qApp` a `qApp` o `qApp` t `qApp` u `qApp` v
    VOptionalBuild a t      -> OptionalBuild `qApp` a `qApp` t
    VRecord m               -> Record (quoteL <$> m)
    VRecordLit m            -> RecordLit (quoteL <$> m)
    VUnion m                -> Union ((quoteL <$>) <$> m)
    VCombine t u            -> Combine (quoteL t) (quoteL u)
    VCombineTypes t u       -> CombineTypes (quoteL t) (quoteL u)
    VPrefer t u             -> Prefer (quoteL t) (quoteL u)
    VMerge t u ma           -> Merge (quoteL t) (quoteL u) ((,ElabAnnot) . quoteL <$> ma)
    VToMap t ma             -> ToMap (quoteL t) ((,ElabAnnot) . quoteL <$> ma)
    VProject t p            -> Project (quoteL t) $ case p of
                                 VProjSingle k -> ProjSingle k
                                 VProjSet k mu -> ProjSet k (quoteL <$> mu)
    VInject t k Nothing     -> Inject (quoteL t) k InjEnum
    VInject t k (Just u)    -> Inject (quoteL t) k InjField `qApp` u
    VPrimVar                -> error evalError


-- Normalization
----------------------------------------------------------------------------------------------------

-- | Normalize an expression in an environment of values. Any variable pointing out of
--   the environment is treated as opaque free variable.
nf :: Env -> Int -> QuoteOption -> Expr I -> Expr X
nf !env !lvl !opt = quote opt lvl . eval env

-- | Normalize an expression in an empty environment.
nfEmpty :: QuoteOption -> Expr I -> Expr X
nfEmpty = nf Empty 0
