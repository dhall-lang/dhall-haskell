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
  -fno-warn-unused-matches
  #-}

{-|

NbE-style elaboration.

Elaboration outputs expressions which

- are well-typed
- are stripped of `Note`-s.
- contain imports which are resolved and type-checked, with imports annotated
  with lazy values of imported expressions.

Not supported currently:
- informative type errors in most cases
- local import caches
- hash integrity checks
- Dot import graphs
- import alternatives
-}

module Dhall.Elaboration where

import Dhall.Core
    ( Expr(..)
    , Binding(..)
    , Chunks(..)
    , Const(..)
    , Var(..)
    , coerceEmbed
    , coerceNote
    , freeIn
    )

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Dynamic
import Data.Foldable
import Data.String
import Dhall.Import
import Data.List.NonEmpty (NonEmpty(..), cons)
import Data.Sequence (pattern (:<|))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Dhall.Eval
import Dhall.Pretty (layoutOpts)
import Dhall.TypeErrors
import Dhall.Parser.Combinators (Src)
import Dhall.Context

import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Dhall.Map
import qualified Dhall.Util
import qualified Data.Sequence

-- Type errors
--------------------------------------------------------------------------------

-- | A structured type error that includes context
data TypeError = TypeError
    { context     :: Cxt
    , current     :: Raw
    , typeMessage :: TypeMessage
    }
    | TmpError (Maybe Src)
    | ImportError MissingImports

instance Show TypeError where
    show = Pretty.renderString . Pretty.layoutPretty layoutOpts . Pretty.pretty

instance Exception TypeError

instance Pretty TypeError where
    pretty (TypeError ctx expr msg)
        = Pretty.unAnnotate
            (   "\n"
            <>  (if null (typesToList $ _types ctx)
                 then ""
                 else prettyContext ctx <> "\n\n")
            <>  shortTypeMessage msg <> "\n"
            <>  source
            )
      where
        prettyKV (key, val) =
            pretty key <> " : " <> Dhall.Util.snipDoc (pretty val)

        prettyContext =
                Pretty.vsep
            .   map prettyKV
            .   reverse
            .   typesToList
            .   _types

        source = case expr of
            Note s _ -> pretty s
            _        -> mempty
    pretty (TmpError s)    = "temporary error:\n" <> pretty s
    pretty (ImportError e) = fromString (show e)

{-| Newtype used to wrap error messages so that they render with a more
    detailed explanation of what went wrong
-}
newtype DetailedTypeError = DetailedTypeError TypeError
    deriving (Typeable)

instance Show DetailedTypeError where
    show = Pretty.renderString . Pretty.layoutPretty layoutOpts . Pretty.pretty

instance Exception DetailedTypeError

instance Pretty DetailedTypeError where
    pretty (DetailedTypeError (TypeError ctx expr msg))
        = Pretty.unAnnotate
            (   "\n"
            <>  (if null (typesToList $ _types ctx)
                 then ""
                 else prettyContext ctx <> "\n\n")
            <>  longTypeMessage msg <> "\n"
            <>  "────────────────────────────────────────────────────────────────────────────────\n"
            <>  "\n"
            <>  source
            )
      where
        prettyKV (key, val) =
            pretty key <> " : " <> Dhall.Util.snipDoc (pretty val)

        prettyContext =
                Pretty.vsep
            .   map prettyKV
            .   reverse
            .   typesToList
            .   _types

        source = case expr of
            Note s _ -> pretty s
            _        -> mempty
    pretty (DetailedTypeError (TmpError s)) = "temporary error:\n" <> pretty s
    pretty (DetailedTypeError (ImportError e)) = fromString (show e)

-- Elaboration
--------------------------------------------------------------------------------

axiom :: Const -> ElabM Const
axiom Type = pure Kind
axiom Kind = pure Sort
axiom Sort = throwM (TypeError emptyCxt (Const Sort) Untyped)

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

tmpError :: ElabM a
tmpError = throwM (TmpError Nothing)

fresh :: Cxt -> Text -> (Text, Val)
fresh Cxt{..} x = (x, VVar x (countName x _values))
{-# inline fresh #-}

unify :: Cxt -> Val -> Val -> ElabM ()
unify Cxt{..} t u = unless (conv _values t u) (tmpError)
{-# inline unify #-}

addNote :: Src -> ElabM a -> ElabM a
addNote s ma = ma `catch` \(err :: TypeError) -> case err of
  TypeError _ Note{} _ -> throwM err
  TypeError cxt' t' m  -> throwM (TypeError cxt' (Note s t') m)
  TmpError (Just s')   -> throwM (TmpError (Just s'))
  TmpError Nothing     -> throwM (TmpError (Just s))
  ImportError e -> do
    -- liftIO $ print $ pretty s
    throwM (ImportError e)

checkTy ::
     Cxt
  -> Raw
  -> Maybe (Core -> TypeError)
  -> ElabM (Core, Const)
checkTy cxt t err = do
  (t, a) <- infer cxt t
  case a of
    VConst c -> pure (t, c)
    _        -> case err of
      Just err -> throwM (err t)
      _        -> tmpError
{-# inline checkTy #-}

check ::
     Cxt
  -> Raw
  -> Val
  -> Maybe (Core -> Val -> TypeError)
  -> ElabM Core
check cxt@Cxt{..} t a err = case (t, a) of
  (Lam x a t, VAnyPi x' a' b) -> do
    (a, _) <- checkTy cxt a Nothing
    let ~av = eval _values a
    let (x', v) = fresh cxt x
    unify cxt av a'
    t <- check (define x' v av cxt) t (b v) Nothing
    pure (Lam x a t)

  (RecordLit ts, VRecord as) -> do
    ts <- flip Dhall.Map.unorderedTraverseWithKey ts $ \k t ->
      maybe (tmpError) (\a -> check cxt t a Nothing) (Dhall.Map.lookup k as)
    pure (RecordLit ts)

  (Merge t u ma, a) -> do
    ma <- forM ma $ \a' -> do
      (a', ak) <- checkTy cxt a' Nothing
      unify cxt a (eval _values a')
      pure a'
    (u, ut) <- infer cxt u
    handlersTy <- case ut of
      VUnion as -> pure (VRecord (maybe a (\b -> vFun b a) <$> as))
      _         -> tmpError
    t <- check cxt t handlersTy Nothing
    pure (Merge t u ma)

  (e@(Let bs t), a) -> do
    (bs, cxt) <- inferBindings cxt bs e
    Let bs <$> check cxt t a Nothing

  (Note s t, a) ->
    addNote s (check cxt t a err)

  (t, a) -> do
    (t, a') <- infer cxt t
    unless (conv _values a a') $ do
      maybe (tmpError)
            (\err -> throwM (err t a'))
            err
    pure t

inferBindings :: Cxt -> NonEmpty RawBinding -> Raw -> ElabM (NonEmpty CoreBinding, Cxt)
inferBindings cxt topBs topT = go cxt topBs where
  goBinding cxt@Cxt{..} (Binding x ma t) = do
    case ma of
      Nothing -> do
        (t, tt) <- infer cxt t
        pure (Binding x Nothing t, define x (eval _values t) tt cxt)
      Just a  -> do
        (a, ak) <- checkTy cxt a Nothing
        let ~av = eval _values a
        t <- check cxt t av
           (Just $ \t a' ->
               TypeError cxt (Let topBs topT) (AnnotMismatch t a (quoteCxt cxt a')))
        pure (Binding x (Just a) t, define x (eval _values t) av cxt)
  go cxt (b :| (b':bs')) = do
    (b, cxt) <- goBinding cxt b
    (bs, cxt) <- go cxt (b' :| bs')
    pure (cons b bs, cxt)
  go cxt (b :| []) = do
    (b, cxt) <- goBinding cxt b
    pure (b :| [], cxt)

tyOfTy :: Cxt -> Val -> ElabM Const
tyOfTy cxt@Cxt{..} a =
  snd <$> checkTy cxt (coerceNote $ coerceEmbed $ quoteCxt cxt a) Nothing
{-# inline tyOfTy #-}

checkTyOfTy :: Cxt -> Val -> Const -> ElabM ()
checkTyOfTy cxt@Cxt{..} a k =
  () <$ check cxt (coerceNote $ coerceEmbed $ quoteCxt cxt a) (VConst k) Nothing
{-# inline checkTyOfTy #-}

natFoldTy :: Val
natFoldTy =
  VHPi "natural" vType $ \natural ->
  VHPi "succ" (vFun natural natural) $ \succ ->
  VHPi "zero" natural $ \zero ->
  natural

listFoldTy :: Val -> Val
listFoldTy a =
  VHPi "list" vType $ \list ->
  VHPi "cons" (vFun a $ vFun list list) $ \cons ->
  VHPi "nil"  list $ \nil ->
  list
{-# inline listFoldTy #-}

optionalFoldTy :: Val ->  Val
optionalFoldTy a =
  VHPi "optional" vType $ \optional ->
  VHPi "some" (vFun a optional) $ \some ->
  VHPi "none" optional $ \none ->
  optional
{-# inline optionalFoldTy #-}

checkCombine :: Cxt -> Val -> Val -> ElabM ()
checkCombine cxt = go where
  go (VRecord as) (VRecord bs) =
    forM_ (Dhall.Map.intersectionWith (,) as bs) (uncurry go)
  go _ _ = tmpError

inferUnion ::
     Cxt
  -> Maybe Const
  -> Dhall.Map.Map Text (Maybe Raw)
  -> ElabM (Core, Val)
inferUnion cxt mak ts = do
  (ts, mak) <- (`runStateT` Nothing) $ (`Dhall.Map.unorderedTraverseWithKey` ts) $
    \k ma -> StateT $ \mak -> case (ma, mak) of
      (Just a,  Just ak) -> do {a <- check cxt a (VConst ak) Nothing; pure (Just a, Just ak)}
      (Just a,  Nothing) -> do {(a, ak) <- checkTy cxt a Nothing; pure (Just a, Just ak)}
      (Nothing, Nothing) -> pure (Nothing, Nothing)
      (Nothing, Just ak) -> pure (Nothing, Just ak)
  pure (Union ts, maybe vType VConst mak)

infer :: Cxt -> Raw -> ElabM (Core, Val)
infer cxt@Cxt{..} t =
  let
    quote_ = quote (envNames _values)
    {-# inline quote_ #-}
    quoteBind_ x = quote (NBind (envNames _values) x)
    {-# inline quoteBind_ #-}
    infer_ = infer cxt
    {-# inline infer_ #-}
    check_ = check cxt
    {-# inline check_ #-}
    checkTy_ = checkTy cxt
    {-# inline checkTy_ #-}
    eval_ = eval _values
    {-# inline eval_ #-}
    checkTyOfTy_ = checkTyOfTy cxt
    {-# inline checkTyOfTy_ #-}
    tyOfTy_ = tyOfTy cxt
    {-# inline tyOfTy_ #-}
    unify_ = unify cxt
    {-# inline unify_ #-}

  in case t of
    Const k -> (\k' -> (Const k, VConst k')) <$> axiom k

    e@(Var (V topX topI)) -> go _types topI where
      go TEmpty          !_ = throwM (TypeError cxt e (UnboundVariable topX))
      go (TBind ts x a) i
        | x == topX = if i == 0 then pure (Var (V topX topI), a) else go ts (i - 1)
        | otherwise = go ts i

    -- Inefficient. Checking lambdas is greatly preferable.
    e@(Lam x a t) -> do
      (a, ak) <- checkTy_ a (Just (TypeError cxt e . InvalidInputType))
      let ~av = eval_ a
      (t, b) <- infer (bind x av cxt) t
      let ~nb = coerceEmbed $ quoteBind_ x b
      bk <- snd <$> checkTy (bind x av cxt) (coerceNote nb) Nothing
      case rule ak bk of
        Just{}  -> pure ()
        Nothing -> throwM (TypeError cxt e (NoDependentTypes a nb))
      case ak of
        -- Fast path for inferring return type, when it cannot be dependent.
        Type -> pure (Lam x a t, vFun av b)
        -- Otherwise we need to normalize the codomain type.
        _ -> pure (
            Lam x a t
          , VHPi x av $ \u ->
              eval (Extend _values x u) nb)

    e@(Pi x a b) -> do
      (a, ak) <- checkTy_ a (Just (TypeError cxt e . InvalidInputType))
      let ~av = eval_ a
      (b, bk) <- checkTy (bind x av cxt) b (Just (TypeError cxt e . InvalidOutputType))
      case rule ak bk of
        Just k' -> pure (Pi x a b, VConst k')
        Nothing -> throwM (TypeError cxt e (NoDependentTypes a b))

    e@(App t u) -> do
      (t, tt) <- infer_ t
      case tt of
        VAnyPi x a b -> do
          u <- check_ u a
                 (Just $ \u a' -> TypeError cxt e
                   (TypeMismatch t (quote_ a) u (quote_ a')))
          pure (App t u, b (eval_ u))
        tt -> throwM (TypeError cxt e (NotAFunction t (quote_ tt)))

    e@(Let bs t) -> do
      (bs, cxt) <- inferBindings cxt bs e
      (t, tt) <- infer cxt t
      pure (Let bs t, tt)

    Annot t a -> do
      (a, ak) <- checkTy_ a Nothing
      let ~av = eval_ a
      t <- check_ t av Nothing
      pure (Annot t a, av)

    Bool        -> pure (Bool, VConst Type)
    BoolLit b   -> pure (BoolLit b, VBool)
    BoolAnd t u -> do
      t <- check_ t VBool Nothing
      u <- check_ u VBool Nothing
      pure (BoolAnd t u, VBool)
    BoolOr t u -> do
      t <- check_ t VBool Nothing
      u <- check_ u VBool Nothing
      pure (BoolOr t u, VBool)
    BoolEQ t u -> do
      t <- check_ t VBool Nothing
      u <- check_ u VBool Nothing
      pure (BoolEQ t u, VBool)
    BoolNE t u -> do
      t <- check_ t VBool Nothing
      u <- check_ u VBool Nothing
      pure (BoolNE t u, VBool)
    BoolIf t u v -> do
      t <- check_ t VBool Nothing
      (u, a) <- infer_ u
      v <- check_ v a Nothing
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
      t <- check_ t VNatural Nothing
      u <- check_ u VNatural Nothing
      pure (NaturalPlus t u, VNatural)
    NaturalTimes t u -> do
      t <- check_ t VNatural Nothing
      u <- check_ u VNatural Nothing
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
      xys <- forM xys $ \(x, t) -> (x,) <$> check_ t VText Nothing
      pure (TextLit (Chunks xys z), VText)

    TextAppend t u -> do
      t <- check_ t VText Nothing
      u <- check_ u VText Nothing
      pure (TextAppend t u, VText)

    TextShow -> pure (TextShow, vFun VText VText)

    List -> pure (List, vFun vType vType)

    ListLit Nothing Data.Sequence.Empty -> tmpError
    ListLit Nothing (t :<| ts) -> do
      (t, a) <- infer_ t
      ts <- forM ts $ \t -> check_ t a Nothing
      pure (ListLit Nothing (t :<| ts), VList a)
    ListLit (Just a) ts -> do
      a <- check_ a vType Nothing
      let ~av = eval_ a
      ts <- forM ts $ \t -> check_ t av Nothing
      pure (ListLit (Just a) ts, VList av)

    ListAppend t u -> do
      (t, tt) <- infer_ t
      case tt of
        VList a -> do
          u <- check_ u (VList a) Nothing
          pure (ListAppend t u, VList a)
        _ -> tmpError

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

    OptionalLit a t -> do
      a <- check_ a vType Nothing
      let ~av = eval_ a
      t <- forM t $ \t -> check_ t av Nothing
      pure (OptionalLit a t, VOptional av)

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
          (a, ak) <- checkTy_ a Nothing
          rest <- flip Dhall.Map.unorderedTraverseWithKey rest $ \k a' -> do
            check_ a' (VConst ak) Nothing
          pure (Record (Dhall.Map.cons k a rest), VConst ak)

    -- this is fairly expensive, it's much better to check record literals than
    -- to infer them. The
    RecordLit ts ->
      case Dhall.Map.uncons ts of
        Nothing -> pure (RecordLit mempty, VRecord mempty)
        Just (k, t, rest) -> do
          (t, a)  <- infer_ t
          ak <- tyOfTy_ a
          rest <- flip Dhall.Map.unorderedTraverseWithKey rest $ \k t' -> do
            (t', a') <- infer_ t'
            checkTyOfTy_ a' ak
            pure (t', a')
          pure ( RecordLit (Dhall.Map.cons k t (fst <$> rest))
               , VRecord (Dhall.Map.cons k a (snd <$> rest)))


    Union ts -> inferUnion cxt Nothing ts

    UnionLit k t as -> do
      case Dhall.Map.lookup k as of
        Just _  -> tmpError -- duplicate alternative
        Nothing -> pure ()
      (t, a) <- infer_ t
      let na = coerceNote $ coerceEmbed $ quote_ a
      (_, ak)       <- checkTy_ na Nothing
      (Union as, _) <- inferUnion cxt (Just ak) as
      pure (UnionLit k t as,
            VUnion ((eval_ <$>) <$> Dhall.Map.insert k (Just na) as))

    Combine t u -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      checkCombine cxt tt ut
      ak <- tyOfTy_ tt
      checkTyOfTy_ ut ak
      pure (Combine t u, vCombineTypes tt ut)

    CombineTypes a b -> do
      (a, ak) <- checkTy_ a Nothing
      b <- check_ b (VConst ak) Nothing
      checkCombine cxt (eval_ a) (eval_ b)
      pure (CombineTypes a b, VConst ak)

    Prefer t u -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      ty <- case (tt, ut) of
        (VRecord as, VRecord bs) -> pure (VRecord (Dhall.Map.union as bs))
        _                        -> tmpError
      ak <- tyOfTy_ tt
      checkTyOfTy_ ut ak
      pure (Prefer t u, ty)

    Merge t u (Just a) -> do
      (a, ak) <- checkTy_ a Nothing
      let ~av = eval_ a
      t <- check_ (Merge t u Nothing) av Nothing

      pure (t, av)

    Merge t u Nothing -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      (handle, union) <- case (tt, ut) of
        (VRecord ts, VUnion us) -> pure (ts, us)
        _                       -> tmpError
      a <- case Dhall.Map.uncons handle of
        Nothing -> tmpError -- missing return type
        Just (k, a, _) ->
          case Dhall.Map.lookup k union of
            Nothing           -> tmpError -- unused handler
            Just Nothing      -> pure a
            Just (Just field) -> case a of
              VAnyPi (fresh cxt -> (_, VVar x i)) field' b -> do
                let bv = b (VVar x i)
                when (freeIn (V x i) (quoteBind_ x bv)) $
                  tmpError   -- can't be a dependent function
                unify_ field field'
                pure bv
              _ -> tmpError
      let tt' = VRecord (maybe a (\field -> vFun field a) <$> union)
      unify_ tt tt'
      pure (Merge t u Nothing, a)

    Field t k -> do
      (t, tt) <- infer_ t
      case tt of
        VRecord ts -> case Dhall.Map.lookup k ts of
          Just a -> pure (Field t k, a)
          _      -> tmpError
        VConst _ -> case eval_ t of
          VUnion ts -> case Dhall.Map.lookup k ts of
            Nothing       -> tmpError
            Just Nothing  -> pure (Field t k, VUnion ts)
            Just (Just a) -> pure (Field t k, vFun a (VUnion ts))
          _ -> tmpError
        _ -> tmpError

    Project t ks -> do
      (t, tt) <- infer_ t
      case tt of
        VRecord as -> do
          as <- fmap Dhall.Map.fromList $ forM (toList ks) $ \k ->
            case Dhall.Map.lookup k as of
              Just a  -> pure (k, a)
              Nothing -> tmpError
          pure (Project t ks, VRecord as)
        _ -> tmpError

    Note s t -> do
      addNote s (infer_ t)

    Embed imp -> do
      (t, tv, a) <- resolve imp `catch` \e -> throwM (ImportError e)
      pure (Embed (Resolved imp t tv), a)

    ImportAlt t u -> do
      infer_ t `catch` \e -> case e of
        ImportError{} -> infer_ u       -- TODO: accumulation of import errors
        err           -> throwM err

infer0 :: FilePath -> Raw -> IO (Core, Val)
infer0 path t = runReaderT (infer emptyCxt t) =<< emptyImportState path

check0 :: FilePath -> Raw -> Val -> IO Core
check0 path t a = runReaderT (check emptyCxt t a Nothing) =<< emptyImportState path
