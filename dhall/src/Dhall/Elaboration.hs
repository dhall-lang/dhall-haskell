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

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Dynamic
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..), cons)
import Data.Map (Map)
import Data.Semigroup
import Data.Sequence (Seq, pattern Empty, pattern (:<|))
import Data.Text (Text)
import Dhall.Binary (StandardVersion(..))
import Dhall.Eval
import Dhall.Parser (Parser(..), ParseError(..), Src(..), SourcedException(..))
import Dhall.TypeErrors
import Lens.Family.State.Strict (zoom)
import Text.Dot
import Data.IORef

import qualified Data.Text as Text
import qualified Dhall.Map


-- Types
--------------------------------------------------------------------------------

-- | An import which has been previously resolved during elaboration. The contained 'Val'
--   is the lazy value of the imported expression.
data ResolvedImport = ResolvedImport !Import Val

-- | Types of local bindings.
data Types = TEmpty | TBind !Types {-# unpack #-} !Text Val

data Cxt = Cxt {
    _values :: !Env
  , _types  :: !Types
  }

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

--------------------------------------------------------------------------------

-- | A structured type error that includes context
data TypeError = TypeError
    { context     :: Cxt
    , current     :: Expr Src Import
    , typeMessage :: TypeMessage
    }

data Error = Error deriving Show
instance Exception Error

--------------------------------------------------------------------------------

axiom :: Const -> ElabM Const
axiom Type = pure Kind
axiom Kind = pure Sort
axiom Sort = throwM Error

rule :: Const -> Const -> ElabM Const
rule Type Type = pure Type
rule Kind Type = pure Type
rule Sort Type = pure Type
rule Kind Kind = pure Kind
rule Sort Kind = pure Sort
rule Sort Sort = pure Sort
-- This forbids dependent types. If this ever changes, then the fast
-- path in the type inference for lambdas becomes unsound.
rule _    _    = throwM Error

fresh :: Cxt -> Text -> (Text, Val)
fresh Cxt{..} x = (x, VVar x (countName x _values))
{-# inline fresh #-}

unify :: Cxt -> Val -> Val -> ElabM ()
unify Cxt{..} t u = unless (conv _values t u) (throwM Error)
{-# inline unify #-}

checkTy :: Cxt -> Expr Src Import -> ElabM (Expr X Resolved, Const)
checkTy cxt t = do
  (t, a) <- infer cxt t
  case a of
    VConst c -> pure (t, c)
    _        -> throwM Error
{-# inline checkTy #-}

check :: Cxt -> Expr Src Import -> Val -> ElabM (Expr X Resolved)
check cxt@Cxt{..} t a = case (t, a) of
  (Lam x a t, VAnyPi x' a' b) -> do
    (a, _) <- checkTy cxt a
    let ~av = eval _values a
    let (x', v) = fresh cxt x
    unify cxt av a'
    t <- check (define x' v av cxt) t (b v)
    pure (Lam x a t)

  (RecordLit ts, VRecord as) -> do
    ts <- flip Dhall.Map.unorderedTraverseWithKey ts $ \k t ->
      maybe (throwM Error) (check cxt t) (Dhall.Map.lookup k as)
    pure (RecordLit ts)

  (Merge t u ma, a) -> do
    ma <- forM ma $ \a' -> do
      (a', ak) <- checkTy cxt a'
      unify cxt a (eval _values a')
      pure a'
    (u, ut) <- infer cxt u
    handlersTy <- case ut of
      VUnion as -> pure (VRecord (maybe a (\a -> vFun ut a) <$> as))
      _         -> throwM Error
    t <- check cxt t handlersTy
    pure (Merge t u ma)

  (Let bs t, a) -> do
    (bs, cxt) <- inferBindings cxt bs
    check cxt t a

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
        (a, ak) <- checkTy cxt a
        let ~av = eval _values a
        t <- check cxt t av
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
  snd <$> checkTy cxt (coerceNote $ coerceEmbed $ quote (envNames _values) a)
{-# inline tyOfTy #-}

checkTyOfTy :: Cxt -> Val -> Const -> ElabM ()
checkTyOfTy cxt@Cxt{..} a k =
  () <$ check cxt (coerceNote $ coerceEmbed $ quote (envNames _values) a) (VConst k)
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
      (Just a,  Just ak) -> do {a <- check cxt a (VConst ak); pure (Just a, Just ak)}
      (Just a,  Nothing) -> do {(a, ak) <- checkTy cxt a; pure (Just a, Just ak)}
      (Nothing, Nothing) -> pure (Nothing, Nothing)
      (Nothing, Just ak) -> pure (Nothing, Just ak)
  pure (Union ts, maybe vType VConst mak)

infer :: Cxt -> Expr Src Import -> ElabM (Expr X Resolved, Val)
infer cxt@Cxt{..} = \case

  Const k -> (\k' -> (Const k, VConst k')) <$> axiom k

  Var (V x i) -> go _types i where
    go TEmpty !_ = throwM Error
    go (TBind ts x' a) i
      | x == x'   = if i == 0 then pure (Var (V x i), a) else go ts (i - 1)
      | otherwise = go ts i

  Lam x a t -> do
    (a, ak) <- checkTy cxt a
    let ~av = eval _values a
    (t, b) <- infer (bind x av cxt) t
    let ~nb = quote (NBind (envNames _values) x) b
    case ak of
      -- Fast path for inferring return type, when it cannot be dependent.
      Type -> pure (Lam x a t, vFun av b)
      -- Otherwise we need to normalize the codomain type.
      _ -> pure (
          Lam x a t
        , VHPi x av $ \u ->
            eval (Extend _values x u) (coerceEmbed nb))

  Pi x a b -> do
    (a, ak) <- checkTy cxt a
    let ~av = eval _values a
    (b, bk) <- checkTy (bind x av cxt) b
    !k' <- rule ak bk
    pure (Pi x a b, VConst k')

  App t u -> do
    (t, tt) <- infer cxt t
    case tt of
      VAnyPi x a b -> do
        u <- check cxt u a
        pure (App t u, b (eval _values u))
      _ -> throwM Error

  Let bs t -> do
    (bs, cxt) <- inferBindings cxt bs
    (t, tt) <- infer cxt t
    pure (Let bs t, tt)

  Annot t a -> do
    (a, ak) <- checkTy cxt a
    let ~av = eval _values a
    t <- check cxt t av
    pure (Annot t a, av)

  Bool        -> pure (Bool, VConst Type)
  BoolLit b   -> pure (BoolLit b, VBool)
  BoolAnd t u -> do
    t <- check cxt t VBool
    u <- check cxt u VBool
    pure (BoolAnd t u, VBool)
  BoolOr t u -> do
    t <- check cxt t VBool
    u <- check cxt u VBool
    pure (BoolOr t u, VBool)
  BoolEQ t u -> do
    t <- check cxt t VBool
    u <- check cxt u VBool
    pure (BoolEQ t u, VBool)
  BoolNE t u -> do
    t <- check cxt t VBool
    u <- check cxt u VBool
    pure (BoolNE t u, VBool)
  BoolIf t u v -> do
    t <- check cxt t VBool
    (u, a) <- infer cxt u
    v <- check cxt v a
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
    t <- check cxt t VNatural
    u <- check cxt u VNatural
    pure (NaturalPlus t u, VNatural)
  NaturalTimes t u -> do
    t <- check cxt t VNatural
    u <- check cxt u VNatural
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
    xys <- forM xys $ \(x, t) -> (x,) <$> check cxt t VText
    pure (TextLit (Chunks xys z), VText)

  TextAppend t u -> do
    t <- check cxt t VText
    u <- check cxt u VText
    pure (TextAppend t u, VText)

  TextShow -> pure (TextShow, vFun VText VText)

  List -> pure (List, VConst Type)

  ListLit Nothing Data.Sequence.Empty -> throwM Error
  ListLit Nothing (t :<| ts) -> do
    (t, a) <- infer cxt t
    ts <- forM ts $ \t -> check cxt t a
    pure (ListLit Nothing (t :<| ts), VList a)
  ListLit (Just a) ts -> do
    a <- check cxt a vType
    let ~av = eval _values a
    ts <- forM ts $ \t -> check cxt t av
    pure (ListLit (Just a) ts, VList av)

  ListAppend t u -> do
    (t, tt) <- infer cxt t
    case tt of
      VList a -> do
        u <- check cxt u (VList a)
        pure (ListAppend t u, VList a)
      _ -> throwM Error

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
    a <- check cxt a vType
    let ~av = eval _values a
    t <- forM t $ \t -> check cxt t av
    pure (OptionalLit a t, VOptional av)

  Some t -> do
    (t, tt) <- infer cxt t
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
        (a, ak) <- checkTy cxt a
        rest <- flip Dhall.Map.unorderedTraverseWithKey rest $ \k a' -> do
          check cxt a' (VConst ak)
        pure (Record (Dhall.Map.cons k a rest), VConst ak)

  -- this is fairly expensive, it's much better to check record literals than
  -- to infer them. The
  RecordLit ts ->
    case Dhall.Map.uncons ts of
      Nothing -> pure (RecordLit mempty, VRecord mempty)
      Just (k, t, rest) -> do
        (t, a)  <- infer cxt t
        ak <- tyOfTy cxt a
        rest <- flip Dhall.Map.unorderedTraverseWithKey rest $ \k t' -> do
          (t', a') <- infer cxt t'
          checkTyOfTy cxt a' ak
          pure (t', a')
        pure (RecordLit (Dhall.Map.cons k t (fst <$> rest)), VRecord (snd <$> rest))

  Union ts -> inferUnion cxt Nothing ts

  UnionLit k t ts -> do
    case Dhall.Map.lookup k ts of
      Just _  -> throwM Error -- duplicate alternative
      Nothing -> pure ()
    (t, a)        <- infer cxt t
    ak            <- tyOfTy cxt a
    (Union ts, _) <- inferUnion cxt (Just ak) ts
    pure (UnionLit k t ts, VUnion ((eval _values <$>) <$> Dhall.Map.insert k (Just t) ts))

  Combine t u -> do
    (t, tt) <- infer cxt t
    (u, ut) <- infer cxt u
    ty <- case (tt, ut) of
      (VRecord{}, VRecord{}) -> pure (vCombineTypes tt ut)
      _                      -> throwM Error
    ak <- tyOfTy cxt tt
    checkTyOfTy cxt ut ak
    pure (Combine t u, ty)

  CombineTypes a b -> do
    (a, ak) <- checkTy cxt a
    b <- check cxt b (VConst ak)
    case (eval _values a, eval _values b) of
      (VRecord{}, VRecord{}) -> pure (CombineTypes a b, VConst ak)
      _                      -> throwM Error

  Prefer t u -> do
    (t, tt) <- infer cxt t
    (u, ut) <- infer cxt u
    ty <- case (tt, ut) of
      (VRecord{}, VRecord{}) -> pure (vCombineTypes tt ut)
      _                      -> throwM Error
    ak <- tyOfTy cxt tt
    checkTyOfTy cxt ut ak
    pure (Prefer t u, ty)

  Merge t u (Just a) -> do
    (a, ak) <- checkTy cxt a
    let ~av = eval _values a
    t <- check cxt (Merge t u Nothing) av
    pure (t, av)

  Merge t u Nothing -> do
    (u, ut) <- infer cxt u
    (t, tt) <- infer cxt t
    (union, handle) <- case (ut, tt) of
      (VUnion ts, VRecord us) -> pure (ts, us)
      _                       -> throwM Error
    a <- case Dhall.Map.uncons handle of
      Nothing -> throwM Error -- missing return type
      Just (k, a, _) ->
        case Dhall.Map.lookup k union of
          Nothing       -> throwM Error -- unused handler
          Just Nothing  -> pure a
          Just (Just _) -> case a of
            VAnyPi (fresh cxt -> (_, v)) _ b -> pure (b v)
            _ -> throwM Error
    let tt' = VRecord (maybe a (\a -> vFun ut a) <$> union)
    unify cxt tt tt'
    pure (Merge t u Nothing, a)

  Field t k -> do
    (t, tt) <- infer cxt t
    case tt of
      VRecord ts -> case Dhall.Map.lookup k ts of
        Just a -> pure (Field t k, a)
        _      -> throwM Error
      VConst Type -> case eval _values t of
        VUnion ts -> case Dhall.Map.lookup k ts of
          Nothing       -> throwM Error
          Just Nothing  -> pure (Field t k, VUnion ts)
          Just (Just a) -> pure (Field t k, vFun a (VUnion ts))
        _ -> throwM Error
      _ -> throwM Error

  Project t ks -> do
    (t, tt) <- infer cxt t
    case tt of
      VRecord as -> do
        as <- fmap Dhall.Map.fromList $ forM (toList ks) $ \k ->
          case Dhall.Map.lookup k as of
            Just a  -> pure (k, a)
            Nothing -> throwM Error
        pure (Project t ks, VRecord as)
      _ -> throwM Error

  Note _ t -> infer cxt t

  ImportAlt t u -> undefined

  Embed imp -> undefined
