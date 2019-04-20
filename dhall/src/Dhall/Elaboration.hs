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
  -fno-warn-unused-imports
  #-}

{-|

NbE-style elaboration.

Elaboration outputs expressions which

- are well-typed
- are stripped of `Note`-s.
- contain imports which are resolved and type-checked, with `Import`-s annotated
  with lazy values of imported expressions.

Since elaboration includes import resolution, we need to carry around state for
imports.
-}


module Dhall.Elaboration where

import Dhall.Core
    ( Expr(..)
    , Binding(..)
    , Chunks(..)
    , Const(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Var(..)
    , X(..)
    , coerceEmbed
    , coerceNote
    )

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Dynamic
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..), cons)
import Data.Map (Map)
import Data.Semigroup
import Data.Sequence (Seq, pattern (:<|))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Dhall.Binary (StandardVersion(..))
import Dhall.Eval
import Dhall.Parser (Parser(..), ParseError(..), Src(..), SourcedException(..))
import Dhall.Pretty (Ann, layoutOpts)
import Dhall.TypeErrors
import Lens.Family.State.Strict (zoom)
import Text.Dot

import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Dhall.Map
import qualified Dhall.Util
import qualified Data.Sequence

-- Elaboration context
--------------------------------------------------------------------------------

-- | An import which has been previously resolved during elaboration. The
--   contained 'Val' is the lazy value of the imported expression.
data ResolvedImport = ResolvedImport !Import Val

-- | Types of local bindings.
data Types = TEmpty | TBind !Types {-# unpack #-} !Text Val

typesNames :: Types -> Names
typesNames TEmpty         = NEmpty
typesNames (TBind ts x _) = NBind (typesNames ts) x

-- | Normal types of local bindings.
typesToList :: Types -> [(Text, Expr X X)]
typesToList TEmpty         = []
typesToList (TBind ts x v) = (x, quote (typesNames ts) v): typesToList ts

data Cxt = Cxt {
    _values :: !Env
  , _types  :: !Types
  }

emptyCxt :: Cxt
emptyCxt = Cxt Empty TEmpty

define :: Text -> Val -> Val -> Cxt -> Cxt
define x t a (Cxt ts as) = Cxt (Extend ts x t) (TBind as x a)
{-# inline define #-}

bind :: Text -> Val -> Cxt -> Cxt
bind x a (Cxt ts as) = Cxt (Skip ts x) (TBind as x a)
{-# inline bind #-}

data ImportState = ImportState
  { _stack :: ![Import]
    -- ^ Stack of `Import`s that we've imported along the way to get to the
    -- current point

  , _dot :: !(Dot NodeId)
    -- ^ Graph of all the imports visited so far

  , _nextNodeId :: !Int
    -- ^ Next node id to be used for the dot graph generation

  , _cache :: !(Map Import (NodeId, Val))
    -- ^ Cache of imported expressions with their node id in order to avoid
    --   importing the same expression twice with different values

  , _manager :: !(Maybe Dynamic)
    -- ^ Cache for the HTTP `Manager` so that we only acquire it once

  , _standardVersion :: !StandardVersion
  }

type ElabM = ReaderT (IORef ImportState) IO

getState :: ElabM ImportState
getState = do
  ref <- ask
  liftIO $ readIORef ref
{-# inline getState #-}

putState :: ImportState -> ElabM ()
putState s = do
  ref <- ask
  liftIO $ writeIORef ref s
{-# inline putState #-}

modifyState :: (ImportState -> ImportState) -> ElabM ()
modifyState f = do
  ref <- ask
  liftIO $ modifyIORef' ref f
{-# inline modifyState #-}


-- Type errors
--------------------------------------------------------------------------------

-- | A structured type error that includes context
data TypeError = TypeError
    { context     :: Cxt
    , current     :: Expr Src Import
    , typeMessage :: TypeMessage
    }

instance Show TypeError where
    show = Pretty.renderString . Pretty.layoutPretty layoutOpts . Pretty.pretty

instance Exception TypeError

instance Pretty TypeError where
    pretty (TypeError ctx expr msg)
        = Pretty.unAnnotate
            (   "\n"
            <>  (   if null (typesToList $ _types ctx)
                    then ""
                    else prettyContext ctx <> "\n\n"
                )
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
            <>  (   if null (typesToList $ _types ctx)
                    then ""
                    else prettyContext ctx <> "\n\n"
                )
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

data TmpError = TmpError deriving Show
instance Exception TmpError



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


fresh :: Cxt -> Text -> (Text, Val)
fresh Cxt{..} x = (x, VVar x (countName x _values))
{-# inline fresh #-}

unify :: Cxt -> Val -> Val -> ElabM ()
unify Cxt{..} t u = unless (conv _values t u) (throwM TmpError)
{-# inline unify #-}

checkTy ::
     Cxt
  -> Expr Src Import
  -> Maybe (Expr X Resolved -> TypeError)
  -> ElabM (Expr X Resolved, Const)
checkTy cxt t err = do
  (t, a) <- infer cxt t
  case a of
    VConst c -> pure (t, c)
    _        -> case err of
      Just err -> throwM (err t)
      _        -> throwM TmpError
{-# inline checkTy #-}

check ::
     Cxt
  -> Expr Src Import
  -> Val
  -> Maybe (Expr X Resolved -> Val -> TypeError)
  -> ElabM (Expr X Resolved)
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
      maybe (throwM TmpError) (\a -> check cxt t a Nothing) (Dhall.Map.lookup k as)
    pure (RecordLit ts)

  (Merge t u ma, a) -> do
    ma <- forM ma $ \a' -> do
      (a', ak) <- checkTy cxt a' Nothing
      unify cxt a (eval _values a')
      pure a'
    (u, ut) <- infer cxt u
    handlersTy <- case ut of
      VUnion as -> pure (VRecord (maybe a (\a -> vFun ut a) <$> as))
      _         -> throwM TmpError
    t <- check cxt t handlersTy Nothing
    pure (Merge t u ma)

  (Let bs t, a) -> do
    (bs, cxt) <- inferBindings cxt bs
    check cxt t a Nothing

  (t, a) -> do
    (t, a') <- infer cxt t
    unify cxt a a'
    pure t

inferBindings ::
  Cxt -> NonEmpty (Binding Src Import) -> ElabM (NonEmpty (Binding X Resolved), Cxt)
inferBindings = go where
  goBinding cxt@Cxt{..} (Binding x ma t) = do
    case ma of
      Nothing -> do
        (t, tt) <- infer cxt t
        pure (Binding x Nothing t, define x (eval _values t) tt cxt)
      Just a  -> do
        (a, ak) <- checkTy cxt a Nothing
        let ~av = eval _values a
        t <- check cxt t av Nothing
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
  snd <$> checkTy cxt (coerceNote $ coerceEmbed $ quote (envNames _values) a) Nothing
{-# inline tyOfTy #-}

checkTyOfTy :: Cxt -> Val -> Const -> ElabM ()
checkTyOfTy cxt@Cxt{..} a k =
  () <$ check cxt (coerceNote $ coerceEmbed $ quote (envNames _values) a) (VConst k) Nothing
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
  VHPi "none" a $ \none ->
  optional
{-# inline optionalFoldTy #-}

inferUnion ::
     Cxt
  -> Maybe Const
  -> Dhall.Map.Map Text (Maybe (Expr Src Import))
  -> ElabM (Expr X Resolved, Val)
inferUnion cxt mak ts = do
  (ts, mak) <- (`runStateT` Nothing) $ (`Dhall.Map.unorderedTraverseWithKey` ts) $
    \k ma -> StateT $ \mak -> case (ma, mak) of
      (Just a,  Just ak) -> do {a <- check cxt a (VConst ak) Nothing; pure (Just a, Just ak)}
      (Just a,  Nothing) -> do {(a, ak) <- checkTy cxt a Nothing; pure (Just a, Just ak)}
      (Nothing, Nothing) -> pure (Nothing, Nothing)
      (Nothing, Just ak) -> pure (Nothing, Just ak)
  pure (Union ts, maybe vType VConst mak)

infer :: Cxt -> Expr Src Import -> ElabM (Expr X Resolved, Val)
infer cxt@Cxt{..} t =
  let
    quote_ = quote (envNames _values)
    {-# inline quote_ #-}
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

  in case t of
    Const k -> (\k' -> (Const k, VConst k')) <$> axiom k

    e@(Var (V x i)) -> go _types i where
      go TEmpty          !_ = throwM (TypeError cxt e (UnboundVariable x))
      go (TBind ts x' a) i
        | x == x'   = if i == 0 then pure (Var (V x i), a) else go ts (i - 1)
        | otherwise = go ts i

    -- Highly inefficient. Checking lambdas is greatly preferable.
    e@(Lam x a t) -> do
      (a, ak) <- checkTy_ a (Just (TypeError cxt e . InvalidInputType))
      let ~av = eval_ a
      (t, b) <- infer (bind x av cxt) t
      let ~nb = coerceEmbed $ quote (NBind (envNames _values) x) b
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

    Let bs t -> do
      (bs, cxt) <- inferBindings cxt bs
      (t, tt) <- infer_ t
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

    List -> pure (List, VConst Type)

    ListLit Nothing Data.Sequence.Empty -> throwM TmpError
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
        _ -> throwM TmpError

    ListBuild ->
      pure (ListBuild, VHPi "a" vType $ \a -> vFun (listFoldTy a) (VList a))
    ListFold ->
      pure (ListFold, VHPi "a" vType $ \a -> vFun (VList a) (listFoldTy a))
    ListLength ->
      pure (ListLength, VHPi "a" vType $ \a -> vFun (VList a) VNatural)
    ListHead ->
      pure (ListHead, VHPi "a" vType $ \a -> vFun (VList a) a)
    ListLast ->
      pure (ListLast, VHPi "a" vType $ \a -> vFun (VList a) a)
    ListIndexed ->
      pure (ListIndexed,
            VHPi "a" vType $ \a ->
            vFun (VList a)
                 (VRecord (Dhall.Map.fromList [("index", VNatural), ("value", a)])))
    ListReverse ->
      pure (ListReverse, VHPi "a" vType $ \a -> vFun (VList a) (VList a))

    Optional -> pure (Optional, vType)
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
          pure (RecordLit (Dhall.Map.cons k t (fst <$> rest)), VRecord (snd <$> rest))

    Union ts -> inferUnion cxt Nothing ts

    UnionLit k t ts -> do
      case Dhall.Map.lookup k ts of
        Just _  -> throwM TmpError -- duplicate alternative
        Nothing -> pure ()
      (t, a)        <- infer_ t
      ak            <- tyOfTy_ a
      (Union ts, _) <- inferUnion cxt (Just ak) ts
      pure (UnionLit k t ts, VUnion ((eval_ <$>) <$> Dhall.Map.insert k (Just t) ts))

    Combine t u -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      ty <- case (tt, ut) of
        (VRecord{}, VRecord{}) -> pure (vCombineTypes tt ut)
        _                      -> throwM TmpError
      ak <- tyOfTy_ tt
      checkTyOfTy_ ut ak
      pure (Combine t u, ty)

    CombineTypes a b -> do
      (a, ak) <- checkTy_ a Nothing
      b <- check_ b (VConst ak) Nothing
      case (eval_ a, eval_ b) of
        (VRecord{}, VRecord{}) -> pure (CombineTypes a b, VConst ak)
        _                      -> throwM TmpError

    Prefer t u -> do
      (t, tt) <- infer_ t
      (u, ut) <- infer_ u
      ty <- case (tt, ut) of
        (VRecord{}, VRecord{}) -> pure (vCombineTypes tt ut)
        _                      -> throwM TmpError
      ak <- tyOfTy_ tt
      checkTyOfTy_ ut ak
      pure (Prefer t u, ty)

    Merge t u (Just a) -> do
      (a, ak) <- checkTy_ a Nothing
      let ~av = eval_ a
      t <- check_ (Merge t u Nothing) av Nothing

      pure (t, av)

    Merge t u Nothing -> do
      (u, ut) <- infer_ u
      (t, tt) <- infer_ t
      (union, handle) <- case (ut, tt) of
        (VUnion ts, VRecord us) -> pure (ts, us)
        _                       -> throwM TmpError
      a <- case Dhall.Map.uncons handle of
        Nothing -> throwM TmpError -- missing return type
        Just (k, a, _) ->
          case Dhall.Map.lookup k union of
            Nothing       -> throwM TmpError -- unused handler
            Just Nothing  -> pure a
            Just (Just _) -> case a of
              VAnyPi (fresh cxt -> (_, v)) _ b -> pure (b v)
              _ -> throwM TmpError
      let tt' = VRecord (maybe a (\a -> vFun ut a) <$> union)
      unify cxt tt tt'
      pure (Merge t u Nothing, a)

    Field t k -> do
      (t, tt) <- infer_ t
      case tt of
        VRecord ts -> case Dhall.Map.lookup k ts of
          Just a -> pure (Field t k, a)
          _      -> throwM TmpError
        VConst Type -> case eval_ t of
          VUnion ts -> case Dhall.Map.lookup k ts of
            Nothing       -> throwM TmpError
            Just Nothing  -> pure (Field t k, VUnion ts)
            Just (Just a) -> pure (Field t k, vFun a (VUnion ts))
          _ -> throwM TmpError
        _ -> throwM TmpError

    Project t ks -> do
      (t, tt) <- infer_ t
      case tt of
        VRecord as -> do
          as <- fmap Dhall.Map.fromList $ forM (toList ks) $ \k ->
            case Dhall.Map.lookup k as of
              Just a  -> pure (k, a)
              Nothing -> throwM TmpError
          pure (Project t ks, VRecord as)
        _ -> throwM TmpError

    Note _ t -> undefined

    ImportAlt t u -> undefined

    Embed imp -> undefined
