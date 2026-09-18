{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

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
    , withEvalRunIO
    , readEvalStats
    , readEvalStatsIO
    , ValueKey (..)
    , AtomicKey (..)
    , freshValId
    , freshEnvId
    , evalCacheEnabled
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
import Control.Monad        (when)
import Control.Monad.ST     (RealWorld, stToIO)
import Data.Hashable        (Hashable)
import GHC.Exts             (touch#)
import GHC.IO               (IO (..))
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text            (Text)
import Data.Word            (Word8, Word32, Word64)
import GHC.Natural          (Natural)
import Dhall.Core.DAG       (Builder, NodeId (..), SharingMode (..), internExpr, newBuilder)
import Dhall.Syntax         (Expr)
import GHC.Generics         (Generic)
import GHC.IO.Unsafe        (unsafeDupablePerformIO)
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.HashMap.Strict as HashMap
import qualified GHC.Exts            as Exts
import qualified Unsafe.Coerce       as Unsafe

-- | Monotonic value identity for one evaluation run.
newtype ValId = ValId { unValId :: Word64 }
    deriving (Eq, Ord, Show)

-- | Monotonic environment-frame identity for one evaluation run.
newtype EnvId = EnvId { unEnvId :: Word64 }
    deriving (Eq, Ord, Show)

-- | Memo key for a closure argument. Identified values use their 'ValId';
-- primitives use a structural key so they can share cache entries without
-- allocating an identity.
data ValueKey
    = ArgIdentified {-# UNPACK #-} !Word64
    | ArgAtomic !AtomicKey
    deriving (Eq, Generic)

instance Hashable ValueKey

-- | Structural identity of a primitive (no 'ValId').
data AtomicKey
    = KType
    | KKind
    | KSort
    | KBoolType
    | KBool !Bool
    | KBytesType
    | KNaturalType
    | KNatural !Natural
    | KIntegerType
    | KInteger !Integer
    | KDoubleType
    | KDouble {-# UNPACK #-} !Word64
    | KTextType
    | KDateType
    | KDate !Integer
    | KTimeType
    | KTime !Word8 !Word8 !Word8 !Integer !Int
    | KTimeZoneType
    | KTimeZone !Int !Bool !Text
    deriving (Eq, Generic)

instance Hashable AtomicKey

-- | Closure-instantiation memo key: body node, captured env, binder, argument.
data EvalKey = EvalKey !Word32 !Word64 !Text !ValueKey
    deriving (Eq, Generic)

instance Hashable EvalKey

-- | Type-inference memo key: denoted node, value env, type context.
data InferKey = InferKey !Word32 !Word64 !Word64
    deriving (Eq, Generic)

instance Hashable InferKey

data EvalConfig = EvalConfig
    { enableCache       :: !Bool
    , cacheCap          :: !Int
    , internSharing     :: !SharingMode
      -- | When 'False', every value gets 'ValId' 0 / every env frame
      -- 'EnvId' 0 (A/B: measure identity-allocation tax).
    , assignIds         :: !Bool
      -- | When 'False', 'makeClosure' skips ST interning (A/B).
    , internOnClosure   :: !Bool
      -- | When 'True', count 'mkVal' calls (extra atomic; profiling only).
    , countMkVal        :: !Bool
    }

-- | Default: identity counters on; speed caches off. Enabling caches is still
-- unsafe under threaded Template Haskell: insert-before-force was fixed, but
-- TH compilation of @Dhall.Test.TH@ still blackholes with caches on. Use
-- 'cacheOnConfig' for runtime measurement outside TH.
defaultEvalConfig :: EvalConfig
defaultEvalConfig = EvalConfig
    { enableCache     = False
    , cacheCap        = 10000
    , internSharing   = SharingOn
    , assignIds       = True
    , internOnClosure = True
    , countMkVal      = False
    }

-- | Opt-in bounded memos for runtime measurement (not safe as the TH default).
cacheOnConfig :: EvalConfig
cacheOnConfig = defaultEvalConfig { enableCache = True }

-- | Oracle: caches disabled (zero insertions).
cacheOffConfig :: EvalConfig
cacheOffConfig = defaultEvalConfig { enableCache = False, cacheCap = 0 }

data EvalStats = EvalStats
    { evalHits         :: !Word64
    , evalMisses       :: !Word64
    , quoteHits        :: !Word64
    , quoteMisses      :: !Word64
    , convHits         :: !Word64
    , convMisses       :: !Word64
    , cacheInserts     :: !Int
    , valIdsIssued     :: !Word64
    , envIdsIssued     :: !Word64
    , internBodyCalls  :: !Word64
    , mkValCalls       :: !Word64
    , evalNanos        :: !Word64
    , quoteNanos       :: !Word64
    } deriving (Eq, Show)

data EvalRun = EvalRun
    { erConfig          :: !EvalConfig
    , erValIdBase       :: !Word64
    , erEnvIdBase       :: !Word64
    , erInternBodyCalls :: !Word64
    , erMkValBase       :: !Word64
    , erBuilder         :: !(Builder RealWorld Exts.Any)
    , erEvalMemo        :: !(HashMap.HashMap EvalKey Exts.Any)
    , erInferMemo       :: !(HashMap.HashMap InferKey Exts.Any)
    , erQuoteMemo       :: !(HashMap.HashMap Word64 NodeId)
    , erConvMemo        :: !(HashMap.HashMap (Word64, Word64) Bool)
    , erEvalHits        :: !Word64
    , erEvalMisses      :: !Word64
    , erQuoteHits       :: !Word64
    , erQuoteMisses     :: !Word64
    , erConvHits        :: !Word64
    , erConvMisses      :: !Word64
    , erInserts         :: !Int
    }

-- | Dedicated counters (not fields of 'globalRun') so identity allocation does
-- not rewrite the whole memoizing 'EvalRun', and so 'unsafePerformIO' cannot
-- CAF a single shared 'ValId' for every 'mkVal'.
{-# NOINLINE valIdRef #-}
valIdRef :: IORef Word64
valIdRef = unsafePerformIO (newIORef 1)

{-# NOINLINE envIdRef #-}
envIdRef :: IORef Word64
envIdRef = unsafePerformIO (newIORef 1)

{-# NOINLINE mkValRef #-}
mkValRef :: IORef Word64
mkValRef = unsafePerformIO (newIORef 0)

{-# NOINLINE assignIdsRef #-}
assignIdsRef :: IORef Bool
assignIdsRef = unsafePerformIO (newIORef True)

{-# NOINLINE internOnClosureRef #-}
internOnClosureRef :: IORef Bool
internOnClosureRef = unsafePerformIO (newIORef True)

{-# NOINLINE countMkValRef #-}
countMkValRef :: IORef Bool
countMkValRef = unsafePerformIO (newIORef False)

{-# NOINLINE cacheEnabledRef #-}
cacheEnabledRef :: IORef Bool
cacheEnabledRef = unsafePerformIO (newIORef False)

{-# NOINLINE globalRun #-}
globalRun :: IORef EvalRun
globalRun = unsafePerformIO $ do
    b <- stToIO (newBuilder SharingOn)
    newIORef (emptyRun defaultEvalConfig b)

emptyRun :: EvalConfig -> Builder RealWorld Exts.Any -> EvalRun
emptyRun cfg b = EvalRun
    { erConfig          = cfg
    , erValIdBase       = 1
    , erEnvIdBase       = 1
    , erInternBodyCalls = 0
    , erMkValBase       = 0
    , erBuilder         = b
    , erEvalMemo        = HashMap.empty
    , erInferMemo       = HashMap.empty
    , erQuoteMemo       = HashMap.empty
    , erConvMemo        = HashMap.empty
    , erEvalHits        = 0
    , erEvalMisses      = 0
    , erQuoteHits       = 0
    , erQuoteMisses     = 0
    , erConvHits        = 0
    , erConvMisses      = 0
    , erInserts         = 0
    }

resetRun :: EvalConfig -> Builder RealWorld Exts.Any -> Word64 -> Word64 -> Word64 -> EvalRun
resetRun cfg b valBase envBase mkBase = EvalRun
    { erConfig          = cfg
    , erValIdBase       = valBase
    , erEnvIdBase       = envBase
    , erInternBodyCalls = 0
    , erMkValBase       = mkBase
    , erBuilder         = b
    , erEvalMemo        = HashMap.empty
    , erInferMemo       = HashMap.empty
    , erQuoteMemo       = HashMap.empty
    , erConvMemo        = HashMap.empty
    , erEvalHits        = 0
    , erEvalMisses      = 0
    , erQuoteHits       = 0
    , erQuoteMisses     = 0
    , erConvHits        = 0
    , erConvMisses      = 0
    , erInserts         = 0
    }

-- | Clear caches and rebuild the intern builder, but keep process-wide
-- 'ValId'/'EnvId' counters. Resetting ids would collide with CAF builtin
-- values allocated before the run.
withEvalRun :: EvalConfig -> a -> a
withEvalRun cfg x = unsafePerformIO $ withEvalRunIO cfg (evaluate x)
{-# NOINLINE withEvalRun #-}

-- | IO variant used by profiling entry points that need phase timers.
withEvalRunIO :: EvalConfig -> IO a -> IO a
withEvalRunIO cfg action = do
    b <- stToIO (newBuilder (internSharing cfg))
    writeIORef assignIdsRef (assignIds cfg)
    writeIORef internOnClosureRef (internOnClosure cfg)
    writeIORef countMkValRef (countMkVal cfg)
    writeIORef cacheEnabledRef (enableCache cfg)
    valBase <- readIORef valIdRef
    envBase <- readIORef envIdRef
    mkBase  <- readIORef mkValRef
    atomicModifyIORef' globalRun $ \_ -> (resetRun cfg b valBase envBase mkBase, ())
    action

statsFromRun :: EvalRun -> Word64 -> Word64 -> Word64 -> EvalStats
statsFromRun st valNow envNow mkNow =
    EvalStats
        { evalHits        = erEvalHits st
        , evalMisses      = erEvalMisses st
        , quoteHits       = erQuoteHits st
        , quoteMisses     = erQuoteMisses st
        , convHits        = erConvHits st
        , convMisses      = erConvMisses st
        , cacheInserts    = erInserts st
        , valIdsIssued    = valNow - erValIdBase st
        , envIdsIssued    = envNow - erEnvIdBase st
        , internBodyCalls = erInternBodyCalls st
        , mkValCalls      = mkNow - erMkValBase st
        , evalNanos       = 0
        , quoteNanos      = 0
        }

readEvalStats :: EvalStats
readEvalStats = unsafePerformIO readEvalStatsIO
{-# NOINLINE readEvalStats #-}

readEvalStatsIO :: IO EvalStats
readEvalStatsIO = do
    st <- readIORef globalRun
    valNow <- readIORef valIdRef
    envNow <- readIORef envIdRef
    mkNow  <- readIORef mkValRef
    return (statsFromRun st valNow envNow mkNow)

-- | Keep @a@ alive without evaluating it, so 'unsafePerformIO' cannot be
-- floated into a CAF while also not forcing thunks (needed for lazy 'extend').
keepAlive :: a -> b -> b
keepAlive a b = unsafeDupablePerformIO $ IO $ \s ->
    case touch# a s of
        s' -> (# s', b #)
{-# NOINLINE keepAlive #-}

-- | Cheap cache-enabled check. The argument is kept alive so this cannot CAF.
evalCacheEnabled :: a -> Bool
evalCacheEnabled a =
    keepAlive a (unsafeDupablePerformIO (readIORef cacheEnabledRef))
{-# NOINLINE evalCacheEnabled #-}

-- | Allocate a fresh 'ValId'. The argument is kept alive (not evaluated) so
-- GHC cannot float the call into a process-wide CAF.
freshValId :: a -> ValId
freshValId a = keepAlive a $
    unsafeDupablePerformIO $ do
        assign <- readIORef assignIdsRef
        if not assign
            then return (ValId 0)
            else do
                count <- readIORef countMkValRef
                when count $
                    atomicModifyIORef' mkValRef $ \n -> (n + 1, ())
                atomicModifyIORef' valIdRef $ \i -> (i + 1, ValId i)
{-# NOINLINE freshValId #-}

freshEnvId :: a -> EnvId
freshEnvId a = keepAlive a $
    unsafeDupablePerformIO $ do
        assign <- readIORef assignIdsRef
        if not assign
            then return (EnvId 0)
            else atomicModifyIORef' envIdRef $ \i -> (i + 1, EnvId i)
{-# NOINLINE freshEnvId #-}

-- | Intern a denoted body into this run's builder. Ids are run-local.
internBody :: (Eq a) => Expr t a -> NodeId
internBody e = unsafePerformIO $ do
    intern <- readIORef internOnClosureRef
    if not intern
        then return (NodeId 0)
        else do
            atomicModifyIORef' globalRun $ \st ->
                (st { erInternBodyCalls = erInternBodyCalls st + 1 }, ())
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
