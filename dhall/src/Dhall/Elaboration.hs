{-# LANGUAGE
  BangPatterns,
  CPP,
  LambdaCase,
  OverloadedStrings,
  PatternSynonyms,
  RankNTypes,
  RecordWildCards,
  ScopedTypeVariables,
  TupleSections,
  ViewPatterns
  #-}

{-# OPTIONS_GHC
  -O
  -fno-warn-name-shadowing
  #-}

{-|
TODO:
- explicitly thread universe levels around instead of recomputing,
  remove heterogeneously kinded unify_-s. This requires adding
  more universe annotations to Core.
-}

module Dhall.Elaboration where

import Control.Applicative ((<|>))
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State.Strict (unless, when, forM, StateT(..))
import Data.Foldable (forM_, toList)
import Data.List.NonEmpty (NonEmpty(..), cons)
import Data.Sequence (pattern (:<|), traverseWithIndex)
import Data.Text (Text)

import Dhall.Context (
  Cxt(..), ElabM, Types(..), ImportState(..), ImportOptions(..), CacheEntry(..)
  , quoteCxt, quoteCxtCore, define, bind, emptyCxt, rootState)

import Dhall.Errors (
  ContextualError(..), TypeError(..), ElabError(..)
  , typeError, elabError)

import Dhall.Core
import Dhall.Eval
import Dhall.Import (resolve)
import Dhall.Src (Src)

import qualified Data.Set as Set
import qualified Dhall.Pretty.Internal
import qualified Dhall.Map
import qualified Data.Sequence

-- Elaboration
--------------------------------------------------------------------------------

axiom :: Cxt -> Const -> ElabM Const
axiom cxt = \case
   Type -> pure Kind
   Kind -> pure Sort
   Sort -> typeError cxt Untyped

rule :: Const -> Const -> Maybe Const
rule Type Type = Just Type
rule Kind Type = Just Type
rule Sort Type = Just Type
rule Kind Kind = Just Kind
rule Sort Kind = Just Sort
rule Sort Sort = Just Sort
-- This forbids dependent types. If this ever changes, then the fast
-- path in the type inference for lambdas becomes unsound.
rule _    _    = Nothing

fresh :: Cxt -> Text -> (Text, Val)
fresh Cxt{..} x = (x, VVar x (countName x _values))
{-# inline fresh #-}

unify :: Cxt -> Val -> Val -> Maybe (ElabM X) -> ElabM ()
unify cxt@Cxt{..} t u err =
  unless (conv _values t u) $
    case err of
      Just err -> absurd <$> err
      _        -> typeError cxt $ ConvError (quoteCxtCore cxt t) (quoteCxtCore cxt u)
{-# inline unify #-}

addSrc :: Src -> ElabM a -> ElabM a
addSrc s ma = ma `catch` \(ContextualError imports cxt ms err) ->
  throwM (ContextualError imports cxt (ms <|> Just s) err)

inferTy ::
     Cxt
  -> Raw
  -> Maybe (Core -> ElabM X)
  -> ElabM (Core, Const)
inferTy cxt topT err = do
  (t, a) <- infer cxt topT
  case a of
    VConst c -> pure (t, c)
    _        -> case err of
      Just err -> absurd <$> err t
      _        -> typeError cxt (ExpectedAType (nfToCore $ quoteCxt cxt a))
{-# inline inferTy #-}

check ::
     Cxt
  -> Raw
  -> VType
  -> Maybe (Core -> VType -> ElabM X)
  -> ElabM Core
check cxt@Cxt{..} t a err =
  let
    quote_     = quote (envNames _values)
    quoteCore_ = nfToCore . quote_
    infer_     = infer cxt
    check_     = check cxt
    inferTy_   = inferTy cxt
    unify_     = unify cxt
    eval_      = eval _values
    typeError_ = typeError cxt
    {-# INLINE quote_     #-}
    {-# INLINE quoteCore_ #-}
    {-# INLINE infer_     #-}
    {-# INLINE check_     #-}
    {-# INLINE inferTy_   #-}
    {-# INLINE unify_     #-}
    {-# INLINE eval_      #-}
    {-# INLINE typeError_ #-}

  in case (t, a) of
    (Lam x a t, VAnyPi _ a' b) -> do
      (a, _) <- inferTy_ a (Just (typeError_ . InvalidInputType))
      let ~av = eval_ a
      let (x', v) = fresh cxt x
      unify_ av a' Nothing
      t <- check (define x' v av cxt) t (b v) Nothing
      pure (Lam x a t)

    (RecordLit ts, VRecord as) -> do
      ts <- flip Dhall.Map.unorderedTraverseWithKey ts $ \k t ->
        maybe (typeError_ (UnexpectedRecordField k (quoteCore_ (VRecord as))))
              (\a -> check_ t a Nothing)
              (Dhall.Map.lookup k as)
      pure (RecordLit ts)

    (Merge t u ma, a) -> do
      ma <- forM ma $ \a' -> do
        (a', _) <- inferTy_ a' Nothing
        unify_ a (eval_ a') Nothing
        pure a'
      (u, ut) <- infer_ u
      handlersTy <- case ut of
        VUnion as -> pure (VRecord (maybe a (\b -> vFun b a) <$> as))
        _         -> typeError_ (MustMergeUnion u (quoteCore_ ut))
      t <- check_ t handlersTy
        (Just $ \t a -> typeError_ (MustMergeARecord t (quoteCore_ a)))
      pure (Merge t u ma)

    (Let bs t, a) -> do
      (bs, cxt) <- inferBindings cxt bs
      Let bs <$> check cxt t a Nothing

    (ListLit ma ts, VList a) -> do
      ma <- forM ma $ \a' -> do
        (a', _) <- inferTy_ a' Nothing
        unify_ a (eval_ a') Nothing
        pure a'
      ts <- forM ts $ \t -> check_ t a Nothing
      pure (ListLit ma ts)

    (Some t, VOptional a) -> do
      Some <$> check_ t a Nothing

    (Note s t, a) ->
      addSrc s (check_ t a err)

    (t, a) -> do
      (t, a') <- infer_ t
      unless (conv _values a a') $ do
        maybe (typeError_ (ConvError (quoteCore_ a') (quoteCore_ a)))
              (\err -> absurd <$> err t a')
              err
      pure t

inferBindings :: Cxt -> NonEmpty RawBinding -> ElabM (NonEmpty CoreBinding, Cxt)
inferBindings cxt topBs = go cxt topBs where
  goBinding cxt@Cxt{..} (Binding x ma t) = do
    case ma of
      Nothing -> do
        (t, tt) <- infer cxt t
        pure (Binding x Nothing t, define x (eval _values t) tt cxt)
      Just a  -> do
        (a, _) <- inferTy cxt a Nothing
        let ~av = eval _values a
        t <- check cxt t av
           (Just $ \t a' ->
               typeError cxt (AnnotMismatch t a (quoteCxtCore cxt a')))
        pure (Binding x (Just a) t, define x (eval _values t) av cxt)
  go cxt (b :| (b':bs')) = do
    (b, cxt) <- goBinding cxt b
    (bs, cxt) <- go cxt (b' :| bs')
    pure (cons b bs, cxt)
  go cxt (b :| []) = do
    (b, cxt) <- goBinding cxt b
    pure (b :| [], cxt)

tyOfTy :: Cxt -> VType -> ElabM Const
tyOfTy cxt@Cxt{..} a =
  snd <$> inferTy cxt (nfToRaw $ quoteCxt cxt a) Nothing
{-# inline tyOfTy #-}

checkTyOfTy :: Cxt -> VType -> Const -> Maybe (Core -> Const -> ElabM X) -> ElabM ()
checkTyOfTy cxt@Cxt{..} a k err =
  () <$ check cxt (nfToRaw $ quoteCxt cxt a) (VConst k) (go <$> err)
  where
    go err a (VConst k') = err a k'
    go _   _ _           = error $ internalError "checkTyOfTy: expected a type"
{-# inline checkTyOfTy #-}

natFoldTy :: VType
natFoldTy =
  VHPi "natural" vType $ \natural ->
  VHPi "succ" (vFun natural natural) $ \_succ ->
  VHPi "zero" natural $ \_zero ->
  natural

listFoldTy :: VType -> VType
listFoldTy a =
  VHPi "list" vType $ \list ->
  VHPi "cons" (vFun a $ vFun list list) $ \_cons ->
  VHPi "nil"  list $ \_nil ->
  list
{-# inline listFoldTy #-}

optionalFoldTy :: VType -> VType
optionalFoldTy a =
  VHPi "optional" vType $ \optional ->
  VHPi "some" (vFun a optional) $ \_some ->
  VHPi "none" optional $ \_none ->
  optional
{-# inline optionalFoldTy #-}

checkCombine :: Core -> Core -> (Core -> Val -> ElabM X) -> VType -> VType -> ElabM ()
checkCombine t u err = go where
  go (VRecord as) (VRecord bs) =
    forM_ (Dhall.Map.intersectionWith (,) as bs) (uncurry go)
  go a VRecord{} = absurd <$> err t a
  go _ b         = absurd <$> err u b

inferUnion ::
     Cxt
  -> Maybe (Text, Core, Const)
  -> Dhall.Map.Map Text (Maybe Raw)
  -> ElabM (Core, VType)
inferUnion cxt acc ts = do
  (ts, acc) <- (`runStateT` acc) $ (`Dhall.Map.unorderedTraverseWithKey` ts) $
    \k ma -> StateT $ \acc -> case (ma, acc) of

      (Just a,  Just (k', a', ak')) -> do
        a <- check cxt a (VConst ak') $ Just $ \a ak -> case ak of
          VConst ak -> typeError cxt (AlternativeAnnotationMismatch k a ak k' a' ak')
          _         -> typeError cxt (InvalidAlternativeType k a)
        pure (Just a, Just (k', a', ak'))

      (Just a, Nothing) -> do
        (a, ak) <- inferTy cxt a $ Just $ \a ->
          typeError cxt (InvalidAlternativeType k a)
        pure (Just a, Just (k, a, ak))

      (Nothing, Nothing) ->
        pure (Nothing, Nothing)

      (Nothing, acc) ->
        pure (Nothing, acc)

  pure (Union ts, maybe vType (\(_, _, ak) -> VConst ak) acc)


infer :: Cxt -> Raw -> ElabM (Core, VType)
infer cxt@Cxt{..} t =
  let
    quote_       = quote (envNames _values)
    quoteCore_   = nfToCore . quote_
    quoteBind_ x = quote (NBind (envNames _values) x)
    infer_       = infer cxt
    check_       = check cxt
    inferTy_     = inferTy cxt
    eval_        = eval _values
    checkTyOfTy_ = checkTyOfTy cxt
    tyOfTy_      = tyOfTy cxt
    unify_       = unify cxt
    typeError_   = typeError cxt
    {-# INLINE quote_       #-}
    {-# INLINE quoteCore_   #-}
    {-# INLINE quoteBind_   #-}
    {-# INLINE infer_       #-}
    {-# INLINE check_       #-}
    {-# INLINE inferTy_     #-}
    {-# INLINE eval_        #-}
    {-# INLINE checkTyOfTy_ #-}
    {-# INLINE tyOfTy_      #-}
    {-# INLINE unify_       #-}
    {-# INLINE typeError_   #-}

    cantTmTy :: (Core -> Core -> TypeError) -> Maybe (Core -> Val -> ElabM X)
    cantTmTy err = Just $ \t a -> typeError_ (err t (quoteCore_ a))

  in case t of
    Const k -> (\k' -> (Const k, VConst k')) <$> axiom cxt k

    Var (V topX topI) -> go _types topI where
      go TEmpty         !_ = typeError_ (UnboundVariable topX)
      go (TBind ts x a) i
        | x == topX = if i == 0 then pure (Var (V topX topI), a) else go ts (i - 1)
        | otherwise = go ts i

    -- Checking lambdas is much more efficient than inferring.
    Lam x a t -> do
      (a, ak) <- inferTy_ a (Just (typeError_ . InvalidInputType))
      let av = eval_ a
      (t, b) <- infer (bind x av cxt) t
      let nb = quoteBind_ x b
      bk <- snd <$> inferTy (bind x av cxt) (nfToRaw nb) Nothing
      case rule ak bk of
        Just{}  -> pure ()
        Nothing -> typeError_ (NoDependentTypes a (nfToCore nb))
      case ak of
        -- Fast path for inferring return type, when it cannot be dependent.
        Type -> pure (Lam x a t, vFun av b)
        -- Otherwise we need to normalize the codomain type.
        _ -> pure (
            Lam x a t
          , VHPi x av $ \u -> eval (Extend _values x u) (nfToCore nb))

    Pi x a b -> do
      (a, ak) <- inferTy_ a (Just (typeError_ . InvalidInputType))
      let ~av = eval_ a
      (b, bk) <- inferTy (bind x av cxt) b (Just (typeError_ . InvalidOutputType))
      case rule ak bk of
        Just k' -> pure (Pi x a b, VConst k')
        Nothing -> typeError_ (NoDependentTypes a b)

    App t u -> do
      (t, tt) <- infer_ t
      case tt of
        VAnyPi _ a b -> do
          u <- check_ u a
                 (Just $ \u a' ->
                     typeError_
                       (TypeMismatch t (quoteCore_ a) u (quoteCore_ a')))
          pure (App t u, b (eval_ u))
        tt -> typeError_ (NotAFunction t (quoteCore_ tt))

    Let bs t -> do
      (bs, cxt) <- inferBindings cxt bs
      (t, tt) <- infer cxt t
      pure (Let bs t, tt)

    Annot t a -> do
      (a, _) <- inferTy_ a Nothing
      let ~av = eval_ a
      t <- check_ t av $ Just $ \t a' ->
        typeError_ (AnnotMismatch t a (quoteCore_ a'))
      pure (Annot t a, av)

    Bool        -> pure (Bool, VConst Type)
    BoolLit b   -> pure (BoolLit b, VBool)
    BoolAnd t u -> do
      t <- check_ t VBool $ cantTmTy CantAnd
      u <- check_ u VBool $ cantTmTy CantAnd
      pure (BoolAnd t u, VBool)
    BoolOr t u -> do
      t <- check_ t VBool $ cantTmTy CantOr
      u <- check_ u VBool $ cantTmTy CantOr
      pure (BoolOr t u, VBool)
    BoolEQ t u -> do
      t <- check_ t VBool $ cantTmTy CantEQ
      u <- check_ u VBool $ cantTmTy CantEQ
      pure (BoolEQ t u, VBool)
    BoolNE t u -> do
      t <- check_ t VBool $ cantTmTy CantNE
      u <- check_ u VBool $ cantTmTy CantNE
      pure (BoolNE t u, VBool)
    BoolIf t u v -> do
      t <- check_ t VBool $ cantTmTy InvalidPredicate
      (u, a) <- infer_ u
      checkTyOfTy_ a Type $ Just $ \a ak ->
        typeError_ (IfBranchMustBeTerm True u a (Const ak))
      v <- check_ v a $ Just $ \u a' ->
        typeError_ (IfBranchMismatch t u (quoteCore_ a) (quoteCore_ a'))
      pure (BoolIf t u v, a)

    Natural          -> pure (Natural, VConst Type)
    NaturalLit n     -> pure (NaturalLit n, VNatural)
    NaturalFold      -> pure (NaturalFold, vFun VNatural natFoldTy)
    NaturalBuild     -> pure (NaturalBuild, vFun natFoldTy VNatural)
    NaturalIsZero    -> pure (NaturalIsZero, vFun VNatural VBool)
    NaturalEven      -> pure (NaturalEven, vFun VNatural VBool)
    NaturalOdd       -> pure (NaturalOdd, vFun VNatural VBool)
    NaturalToInteger -> pure (NaturalToInteger, vFun VNatural VInteger)
    NaturalShow      -> pure (NaturalShow, vFun VNatural VText)

    NaturalPlus t u -> do
      t <- check_ t VNatural $ cantTmTy CantAdd
      u <- check_ u VNatural $ cantTmTy CantAdd
      pure (NaturalPlus t u, VNatural)
    NaturalTimes t u -> do
      t <- check_ t VNatural $ cantTmTy CantMultiply
      u <- check_ u VNatural $ cantTmTy CantMultiply
      pure (NaturalTimes t u, VNatural)

    Integer         -> pure (Integer, VConst Type)
    IntegerLit n    -> pure (IntegerLit n, VInteger)
    IntegerShow     -> pure (IntegerShow, vFun VInteger VText)
    IntegerToDouble -> pure (IntegerToDouble, vFun VInteger VDouble)
    Double          -> pure (Double, VConst Type)
    DoubleLit n     -> pure (DoubleLit n, VDouble)
    DoubleShow      -> pure (DoubleShow, vFun VDouble VText)

    Text -> pure (Text, VConst Type)

    TextLit (Chunks xys z) -> do
      xys <- forM xys $ \(x, t) -> do
        t <- check_ t VText $ cantTmTy CantInterpolate
        pure (x, t)
      pure (TextLit (Chunks xys z), VText)

    TextAppend t u -> do
      t <- check_ t VText $ cantTmTy CantTextAppend
      u <- check_ u VText $ cantTmTy CantTextAppend
      pure (TextAppend t u, VText)

    TextShow -> pure (TextShow, vFun VText VText)

    List -> pure (List, vFun vType vType)

    ListLit Nothing Data.Sequence.Empty -> typeError_ MissingListType
    ListLit Nothing (t :<| ts) -> do
      (t, a) <- infer_ t
      checkTyOfTy_ a Type $ Just $ \_ _ ->
        typeError_ (InvalidListType (quoteCore_ a))
      ts <- flip traverseWithIndex ts $ \i t ->
        check_ t a $
          Just $ \t a' ->
            typeError_
              (MismatchedListElements i (quoteCore_ a) t (quoteCore_ a'))
      pure (ListLit Nothing (t :<| ts), VList a)

    ListLit (Just a) ts -> do
      a <- check_ a vType $ Just $ \a _ ->
        typeError_ (InvalidListType a)
      let ~av = eval_ a
      ts <- flip traverseWithIndex ts $ \i t ->
        check_ t av $
          Just $ \t a' ->
            typeError_
              (MismatchedListElements i (quoteCore_ av) t (quoteCore_ a'))
      pure (ListLit (Just a) ts, VList av)

    ListAppend t u -> do
      (t, tt) <- infer_ t
      case tt of
        VList a -> do
          u <- check_ u (VList a) $ cantTmTy CantListAppend
          pure (ListAppend t u, VList a)
        _ -> typeError_ (CantListAppend t (quoteCore_ tt))

    ListBuild ->
      pure (ListBuild, VHPi "a" vType $ \a -> vFun (listFoldTy a) (VList a))
    ListFold ->
      pure (ListFold, VHPi "a" vType $ \a -> vFun (VList a) (listFoldTy a))
    ListLength ->
      pure (ListLength, VHPi "a" vType $ \a -> vFun (VList a) VNatural)
    ListHead ->
      pure (ListHead, VHPi "a" vType $ \a -> vFun (VList a) (VOptional a))
    ListLast ->
      pure (ListLast, VHPi "a" vType $ \a -> vFun (VList a) (VOptional a))
    ListIndexed ->
      pure (ListIndexed,
            VHPi "a" vType $ \a ->
            vFun (VList a)
                 (VList $
                    VRecord (Dhall.Map.fromList [("index", VNatural), ("value", a)])))
    ListReverse ->
      pure (ListReverse, VHPi "a" vType $ \a -> vFun (VList a) (VList a))

    Optional -> pure (Optional, vFun vType vType)
    None     -> pure (None, VHPi "a" vType VOptional)

    Some t -> do
      (t, tt) <- infer_ t
      pure (Some t, VOptional tt)

    OptionalFold -> pure
      (OptionalFold,
       VHPi "a" vType $ \a -> vFun (VOptional a) (optionalFoldTy a))

    OptionalBuild -> pure
      (OptionalBuild,
       VHPi "a" vType $ \a -> vFun (optionalFoldTy a) (VOptional a))

    Record ts ->
      case Dhall.Map.uncons ts of
        Nothing -> pure (Record mempty, vType)
        Just (k, a, rest) -> do

          (a, ak) <- inferTy_ a $ Just $ \a ->
            typeError_ (InvalidFieldType k a)

          rest <- flip Dhall.Map.unorderedTraverseWithKey rest $ \k' a' -> do
            check_ a' (VConst ak) $ Just $ \a' ak' ->
              case ak' of
                VConst ak' ->
                  typeError_ (FieldAnnotationMismatch k' a' ak' k a ak)
                _ ->
                  typeError_ (InvalidFieldType k' a')

          pure (Record (Dhall.Map.insert k a rest), VConst ak)

    -- this is fairly expensive, it's much better to check record literals than
    -- to infer them.
    RecordLit ts ->
      case Dhall.Map.uncons ts of
        Nothing -> pure (RecordLit mempty, VRecord mempty)
        Just (k, t, rest) -> do
          (t, a)  <- infer_ t
          ak      <- tyOfTy_ a
          rest <- flip Dhall.Map.unorderedTraverseWithKey rest $ \k' t' -> do
            (t', a') <- infer_ t'
            checkTyOfTy_ a' ak $ Just $ \_a' ak' ->
              typeError_ (FieldMismatch k' t' ak' k t ak)
            pure (t', a')

          pure ( RecordLit (Dhall.Map.insert k t (fst <$> rest))
               , VRecord   (Dhall.Map.insert k a (snd <$> rest)))


    Union ts -> inferUnion cxt Nothing ts

    UnionLit k t as -> do
      case Dhall.Map.lookup k as of
        Just _  -> typeError_ (DuplicateAlternative k)
        Nothing -> pure ()
      (t, a) <- infer_ t
      ak <- tyOfTy_ a
      let na = quoteCore_ a
      (Union as, _) <- inferUnion cxt (Just (k, na, ak)) as
      pure (UnionLit k t as,
            VUnion ((eval_ <$>) <$> Dhall.Map.insert k (Just na) as))

    Combine t u -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      checkCombine t u
        (\t tt -> typeError_ (MustCombineARecord '∧' t (quoteCore_ tt)))
        tt ut
      ak <- tyOfTy_ tt
      checkTyOfTy_ ut ak $ Just $ \ut ak' ->
        typeError_ (RecordMismatch '∧' (quoteCore_ tt) ut ak ak')

      pure (Combine t u, vCombineTypes tt ut)

    CombineTypes a b -> do
      (a, ak) <- inferTy_ a Nothing
      b <- check_ b (VConst ak) $ Just $ \b bk ->
        case bk of
          VConst bk -> typeError_ (RecordTypeMismatch ak bk a b)
          _         -> typeError_ (CombineTypesRequiresRecordType b (quoteCore_ bk))
      checkCombine a b
        (\a ak -> typeError_ (CombineTypesRequiresRecordType a (quoteCore_ ak)))
        (eval_ a) (eval_ b)
      pure (CombineTypes a b, VConst ak)

    Prefer t u -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      ty <- case (tt, ut) of
        (VRecord as, VRecord bs) -> pure (VRecord (Dhall.Map.union as bs))
        (_, VRecord{}) -> typeError_ (MustCombineARecord '⫽' t (quoteCore_ tt))
        _              -> typeError_ (MustCombineARecord '⫽' u (quoteCore_ ut))
      ak <- tyOfTy_ tt
      checkTyOfTy_ ut ak $ Just $ \ut ak' ->
        typeError_ (RecordMismatch '⫽' (quoteCore_ tt) ut ak ak')
      pure (Prefer t u, ty)

    Merge t u (Just a) -> do
      (a, _) <- inferTy_ a Nothing
      let ~av = eval_ a
      t <- check_ (Merge t u Nothing) av Nothing
      pure (t, av)

    Merge t u Nothing -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      (handle, union) <- case (tt, ut) of
        (VRecord ts, VUnion us) -> pure (ts, us)
        (_ , VUnion{}) -> typeError_ (MustMergeARecord t (quoteCore_ tt))
        _              -> typeError_ (MustMergeUnion u (quoteCore_ ut))
      a <- case Dhall.Map.uncons handle of
        Nothing -> typeError_ MissingMergeType
        Just (k, a, _) ->
          case Dhall.Map.lookup k union of
            Nothing           -> typeError_ (UnusedHandler (Set.singleton k))
            Just Nothing      -> pure a
            Just (Just field) -> case a of
              VAnyPi (fresh cxt -> (_, VVar x i)) field' b -> do
                let bv = b (VVar x i)
                when (freeIn (V x i) (quoteBind_ x bv)) $
                  typeError_ (MergeDependentHandler k (quoteCore_ a))
                unify_ field field' $ Just $
                  typeError_ (HandlerInputTypeMismatch k (quoteCore_ field) (quoteCore_ field'))
                pure bv
              _ -> typeError_ (HandlerNotAFunction k (quoteCore_ a))
      let tt' = VRecord (maybe a (\field -> vFun field a) <$> union)
      unify_ tt tt' Nothing
      pure (Merge t u Nothing, a)

    ToMap t ma -> do
      let recTy fieldTy =
            VRecord $ Dhall.Map.fromList [("mapKey", VText), ("mapValue", fieldTy)]

      (t, tt) <- infer_ t

      -- the second projection of mFieldTy here is the field type
      (mFieldTy :: Maybe (Core, VType)) <- forM ma $ \a -> do
        (a, _) <- inferTy_ a Nothing
        case eval_ a of
          VList (VRecord kts)
            | Just fieldTy <- Dhall.Map.lookup "mapValue" kts,
              Just VText   <- Dhall.Map.lookup "mapKey" kts,
              Dhall.Map.keys kts == ["mapKey", "mapValue"] ->
            pure (a, fieldTy)
          va -> typeError_ (InvalidToMapType (quoteCore_ va))

      kts <- case tt of
        VRecord kts -> pure kts
        _           -> typeError_ (MustMapARecord t (quoteCore_ tt))

      case (Dhall.Map.uncons kts, mFieldTy) of
        (Nothing, Nothing)             -> typeError_ MissingToMapType
        (Nothing, Just (ann, fieldTy)) -> pure (ToMap t (Just ann), VList (recTy fieldTy))
        (Just (_, a, kts), mFieldTy) -> do
          flip Dhall.Map.unorderedTraverseWithKey_ kts $ \_ a' -> do
            unify_ a a' $ Just $
              typeError_ (HeterogeneousRecordToMap (quoteCore_ tt) (quoteCore_ a) (quoteCore_ a'))
          case mFieldTy of
            Just (ann, fieldTy) -> do
              unify_ a fieldTy $ Just $
                typeError_ (MapTypeMismatch (quoteCore_ (VList (recTy a)))
                                            (quoteCore_ (VList (recTy fieldTy))))
              pure (ToMap t (Just ann), VList (recTy a))
            Nothing ->
              pure (ToMap t Nothing, VList (recTy a))

    Field t k -> do
      (t, tt) <- infer_ t
      case tt of
        VRecord ts -> case Dhall.Map.lookup k ts of
          Just a -> pure (Field t k, a)
          _      -> typeError_ (MissingField k (quoteCore_ tt))
        VConst _ -> case eval_ t of
          VUnion ts -> case Dhall.Map.lookup k ts of
            Nothing       -> typeError_ (MissingAlternative k (quoteCore_ tt))
            Just Nothing  -> pure (Field t k, VUnion ts)
            Just (Just a) -> pure (Field t k, vFun a (VUnion ts))
          _ -> typeError_ (CantAccess k t (quoteCore_ tt))
        _ -> typeError_ (CantAccess k t (quoteCore_ tt))

    Project t (Left ks) -> do
      (t, tt) <- infer_ t
      case tt of
        VRecord as -> do
          as <- fmap Dhall.Map.fromList $ forM (toList ks) $ \k ->
            case Dhall.Map.lookup k as of
              Just a  -> pure (k, a)
              Nothing -> typeError_ (MissingField k (quoteCore_ tt))
          pure (Project t (Left ks), VRecord as)
        _ -> let text = Dhall.Pretty.Internal.docToStrictText
                   (Dhall.Pretty.Internal.prettyLabels ks)
             in typeError_ (CantProject text t (quoteCore_ tt))

    Project t (Right u) -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      case tt of
        VRecord as -> case ut of
            VRecord bs -> do
              flip Dhall.Map.unorderedTraverseWithKey_ bs $ \k rTy -> do
                case Dhall.Map.lookup k as of
                  Just lTy -> unify_ lTy rTy $ Just $
                                typeError_ (ProjectionTypeMismatch k (quoteCore_ lTy) (quoteCore_ rTy)
                                            t u)
                  Nothing  -> typeError_ (MissingField k (quoteCore_ tt))
              pure (Project t (Right u), VRecord bs)
            _ ->
              typeError_ (CantProjectByExpression u)
        _ ->
          let text = Dhall.Core.pretty u
          in typeError_ (CantProject text t (quoteCore_ tt))

    Note s t -> do
      addSrc s (infer_ t)

    Embed imp@Import{} -> do
      ImportState{..} <- ask
      case _importOptions of
        ImportsDisabled -> elabError cxt ImportResolutionDisabled
        _               -> pure ()

      (imp@Import{..}, CacheEntry t tv a nf hsh) <- resolve cxt imp
      let hashedImp = imp {importHashed = importHashed {hash = Just hsh}}

      -- _stack check: freezing and unfolding only in the root expression!
      case (_stack, _importOptions, importType importHashed) of
        (_ :| [], FreezeRemote, Remote{}) -> pure (Embed (Resolved hashedImp t tv), a)
        (_ :| [], FreezeAll,    _       ) -> pure (Embed (Resolved hashedImp t tv), a)
        (_ :| [], OldResolve,   _       ) -> pure (nfToCore nf, a)
        _                                 -> pure (Embed (Resolved imp t tv), a)

    ImportAlt t u -> do
      infer_ t `catch` \case
        ContextualError _ _ _ ImportError{} -> infer_ u
        e -> throwM e

inferRoot :: FilePath -> Raw -> IO (Core, Val)
inferRoot path t = runReaderT (infer emptyCxt t) =<< rootState path

checkRoot :: FilePath -> Raw -> Val -> IO Core
checkRoot path t a = runReaderT (check emptyCxt t a Nothing) =<< rootState path
