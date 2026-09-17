{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Per-evaluation-run identity counters and bounded speed caches.
--
-- Identifiers are meaningful only inside one 'withEvalRun' (or one public
-- 'eval'/'quote'/'conv' call). They are never serialized, shown, or hashed.
module Dhall.Eval.Run
    ( ValId (..)
    , EnvId (..)
    , EvalKey (..)
    , InferKey (..)
    , EvalConfig (..)
    , EvalStats (..)
    , defaultEvalConfig
    , cacheOnConfig
    , cacheOffConfig
    , withEvalRun
    , readEvalStats
    , freshValId
    , freshEnvId
    , internBody
    , lookupEvalMemo
    , insertEvalMemo
    , lookupQuoteMemo
    , insertQuoteMemo
    , lookupConvMemo
    , insertConvMemo
    , lookupInferMemo
    , insertInferMemo
    ) where

import Control.Exception    (evaluate)
import Control.Monad.ST     (RealWorld, stToIO)
import Data.Hashable        (Hashable)
import Data.IORef           (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text            (Text)
import Data.Word            (Word32, Word64)
import Dhall.Core.DAG       (Builder, NodeId (..), SharingMode (..), internExpr, newBuilder)
import Dhall.Syntax         (Expr)
import GHC.Generics         (Generic)
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.HashMap.Strict as HashMap
import qualified GHC.Exts            as Exts
import qualified Unsafe.Coerce       as Unsafe

-- | Monotonic value identity for one evaluation run.
newtype ValId = ValId { unValId :: Word64 }
    deriving (Eq, Ord)

-- | Monotonic environment-frame identity for one evaluation run.
newtype EnvId = EnvId { unEnvId :: Word64 }
    deriving (Eq, Ord)

-- | Closure-instantiation memo key: body node, captured env, binder, argument.
data EvalKey = EvalKey !Word32 !Word64 !Text !Word64
    deriving (Eq, Generic)

instance Hashable EvalKey

-- | Type-inference memo key: denoted node, value env, type context.
data InferKey = InferKey !Word32 !Word64 !Word64
    deriving (Eq, Generic)

instance Hashable InferKey

data EvalConfig = EvalConfig
    { enableCache  :: !Bool
    , cacheCap     :: !Int
    , internSharing :: !SharingMode
    }

-- | Default: identity counters on; speed caches off until a non-reentrant
-- memo design is proven under @-threaded@ TH (blackhole risk).
defaultEvalConfig :: EvalConfig
defaultEvalConfig = EvalConfig
    { enableCache   = False
    , cacheCap      = 10000
    , internSharing = SharingOn
    }

-- | Opt-in bounded memos for measurement (not the default).
cacheOnConfig :: EvalConfig
cacheOnConfig = defaultEvalConfig { enableCache = True }

-- | Oracle: caches disabled (zero insertions).
cacheOffConfig :: EvalConfig
cacheOffConfig = defaultEvalConfig { enableCache = False, cacheCap = 0 }

data EvalStats = EvalStats
    { evalHits    :: !Word64
    , evalMisses  :: !Word64
    , quoteHits   :: !Word64
    , quoteMisses :: !Word64
    , convHits    :: !Word64
    , convMisses  :: !Word64
    , cacheInserts :: !Int
    } deriving (Eq, Show)

data EvalRun = EvalRun
    { erConfig      :: !EvalConfig
    , erNextVal     :: !Word64
    , erNextEnv     :: !Word64
    , erBuilder     :: !(Builder RealWorld Exts.Any)
    , erEvalMemo    :: !(HashMap.HashMap EvalKey Exts.Any)
    , erInferMemo   :: !(HashMap.HashMap InferKey Exts.Any)
    , erQuoteMemo   :: !(HashMap.HashMap Word64 NodeId)
    , erConvMemo    :: !(HashMap.HashMap (Word64, Word64) Bool)
    , erEvalHits    :: !Word64
    , erEvalMisses  :: !Word64
    , erQuoteHits   :: !Word64
    , erQuoteMisses :: !Word64
    , erConvHits    :: !Word64
    , erConvMisses  :: !Word64
    , erInserts     :: !Int
    }

{-# NOINLINE globalRun #-}
globalRun :: IORef EvalRun
globalRun = unsafePerformIO $ do
    b <- stToIO (newBuilder SharingOn)
    newIORef (emptyRun defaultEvalConfig b)

emptyRun :: EvalConfig -> Builder RealWorld Exts.Any -> EvalRun
emptyRun cfg b = EvalRun
    { erConfig      = cfg
    , erNextVal     = 1
    , erNextEnv     = 1
    , erBuilder     = b
    , erEvalMemo    = HashMap.empty
    , erInferMemo   = HashMap.empty
    , erQuoteMemo   = HashMap.empty
    , erConvMemo    = HashMap.empty
    , erEvalHits    = 0
    , erEvalMisses  = 0
    , erQuoteHits   = 0
    , erQuoteMisses = 0
    , erConvHits    = 0
    , erConvMisses  = 0
    , erInserts     = 0
    }

-- | Clear caches and rebuild the intern builder, but keep process-wide
-- 'ValId'/'EnvId' counters. Resetting ids would collide with CAF builtin
-- values allocated before the run.
withEvalRun :: EvalConfig -> a -> a
withEvalRun cfg x = unsafePerformIO $ do
    b <- stToIO (newBuilder (internSharing cfg))
    atomicModifyIORef' globalRun $ \st ->
        ( (emptyRun cfg b)
            { erNextVal = erNextVal st
            , erNextEnv = erNextEnv st
            }
        , ()
        )
    evaluate x
{-# NOINLINE withEvalRun #-}

readEvalStats :: EvalStats
readEvalStats = unsafePerformIO $ do
    st <- readIORef globalRun
    return EvalStats
        { evalHits     = erEvalHits st
        , evalMisses   = erEvalMisses st
        , quoteHits    = erQuoteHits st
        , quoteMisses  = erQuoteMisses st
        , convHits     = erConvHits st
        , convMisses   = erConvMisses st
        , cacheInserts = erInserts st
        }
{-# NOINLINE readEvalStats #-}

freshValId :: () -> ValId
freshValId _ = unsafePerformIO $
    atomicModifyIORef' globalRun $ \st ->
        let i = erNextVal st
        in  (st { erNextVal = i + 1 }, ValId i)
{-# NOINLINE freshValId #-}

freshEnvId :: () -> EnvId
freshEnvId _ = unsafePerformIO $
    atomicModifyIORef' globalRun $ \st ->
        let i = erNextEnv st
        in  (st { erNextEnv = i + 1 }, EnvId i)
{-# NOINLINE freshEnvId #-}

-- | Intern a denoted body into this run's builder. Ids are run-local.
internBody :: (Eq a) => Expr t a -> NodeId
internBody e = unsafePerformIO $ do
    st <- readIORef globalRun
    nid <- stToIO (internExpr (Unsafe.unsafeCoerce (erBuilder st)) e)
    return nid
{-# NOINLINE internBody #-}

lookupEvalMemo :: EvalKey -> Maybe v
lookupEvalMemo key = unsafePerformIO $
    atomicModifyIORef' globalRun $ \st ->
        if not (enableCache (erConfig st))
            then (st, Nothing)
            else case HashMap.lookup key (erEvalMemo st) of
                Just v ->
                    ( st { erEvalHits = erEvalHits st + 1 }
                    , Just (Unsafe.unsafeCoerce v)
                    )
                Nothing ->
                    ( st { erEvalMisses = erEvalMisses st + 1 }
                    , Nothing
                    )
{-# NOINLINE lookupEvalMemo #-}

insertEvalMemo :: EvalKey -> v -> v
insertEvalMemo key v = unsafePerformIO $ do
    -- Force the value *before* taking the IORef lock. `atomicModifyIORef'`
    -- seqs its result while holding the lock; evaluating `Val`s there
    -- re-enters these memos and deadlocks (seen as 0% CPU in TH splices).
    v' <- evaluate v
    atomicModifyIORef' globalRun $ \st ->
        let cfg = erConfig st
        in  if not (enableCache cfg) || erInserts st >= cacheCap cfg
            then (st, ())
            else
                ( st
                    { erEvalMemo = HashMap.insert key (Unsafe.unsafeCoerce v') (erEvalMemo st)
                    , erInserts  = erInserts st + 1
                    }
                , ()
                )
    return v'
{-# NOINLINE insertEvalMemo #-}

lookupQuoteMemo :: ValId -> Maybe NodeId
lookupQuoteMemo (ValId i) = unsafePerformIO $
    atomicModifyIORef' globalRun $ \st ->
        if not (enableCache (erConfig st))
            then (st, Nothing)
            else case HashMap.lookup i (erQuoteMemo st) of
                Just nid ->
                    ( st { erQuoteHits = erQuoteHits st + 1 }
                    , Just nid
                    )
                Nothing ->
                    ( st { erQuoteMisses = erQuoteMisses st + 1 }
                    , Nothing
                    )
{-# NOINLINE lookupQuoteMemo #-}

insertQuoteMemo :: ValId -> NodeId -> NodeId
insertQuoteMemo (ValId i) nid = unsafePerformIO $ do
    atomicModifyIORef' globalRun $ \st ->
        let cfg = erConfig st
        in  if not (enableCache cfg) || erInserts st >= cacheCap cfg
            then (st, ())
            else
                ( st
                    { erQuoteMemo = HashMap.insert i nid (erQuoteMemo st)
                    , erInserts   = erInserts st + 1
                    }
                , ()
                )
    return nid
{-# NOINLINE insertQuoteMemo #-}

lookupConvMemo :: ValId -> ValId -> Maybe Bool
lookupConvMemo (ValId a) (ValId b) = unsafePerformIO $
    atomicModifyIORef' globalRun $ \st ->
        if not (enableCache (erConfig st))
            then (st, Nothing)
            else
                let key = if a <= b then (a, b) else (b, a)
                in case HashMap.lookup key (erConvMemo st) of
                    Just r ->
                        ( st { erConvHits = erConvHits st + 1 }
                        , Just r
                        )
                    Nothing ->
                        ( st { erConvMisses = erConvMisses st + 1 }
                        , Nothing
                        )
{-# NOINLINE lookupConvMemo #-}

insertConvMemo :: ValId -> ValId -> Bool -> Bool
insertConvMemo (ValId a) (ValId b) r = unsafePerformIO $ do
    r' <- evaluate r
    atomicModifyIORef' globalRun $ \st ->
        let cfg = erConfig st
            key = if a <= b then (a, b) else (b, a)
        in  if not (enableCache cfg) || erInserts st >= cacheCap cfg
            then (st, ())
            else
                ( st
                    { erConvMemo = HashMap.insert key r' (erConvMemo st)
                    , erInserts  = erInserts st + 1
                    }
                , ()
                )
    return r'
{-# NOINLINE insertConvMemo #-}

lookupInferMemo :: InferKey -> Maybe v
lookupInferMemo key = unsafePerformIO $
    atomicModifyIORef' globalRun $ \st ->
        if not (enableCache (erConfig st))
            then (st, Nothing)
            else case HashMap.lookup key (erInferMemo st) of
                Just v -> (st, Just (Unsafe.unsafeCoerce v))
                Nothing -> (st, Nothing)
{-# NOINLINE lookupInferMemo #-}

insertInferMemo :: InferKey -> v -> v
insertInferMemo key v = unsafePerformIO $ do
    v' <- evaluate v
    atomicModifyIORef' globalRun $ \st ->
        let cfg = erConfig st
        in  if not (enableCache cfg) || erInserts st >= cacheCap cfg
            then (st, ())
            else
                ( st
                    { erInferMemo = HashMap.insert key (Unsafe.unsafeCoerce v') (erInferMemo st)
                    , erInserts   = erInserts st + 1
                    }
                , ()
                )
    return v'
{-# NOINLINE insertInferMemo #-}
