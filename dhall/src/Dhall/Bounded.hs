{-| Normalization and pretty-printing that stop at a fixed output size.

    Ordinary 'Dhall.Core.normalize' is unchanged.  These helpers are for a
    caller that is about to show a normal form: the CLI @--max-output-size@
    flag, and the language server whenever it displays one.

    The output budget is a number of bytes of rendered text (default 128KiB).
    Quoting stops after that many syntax nodes so a huge value is not fully
    converted to syntax.  Evaluation itself is unchanged; the work bound is a
    per-thread allocation limit plus a timeout, run on a dedicated thread.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Bounded
    ( Bound(..)
    , BoundLimits(..)
    , defaultOutputBytes
    , defaultBoundLimits
    , normalizeBounded
    , prettyBounded
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception
    ( AsyncException (ThreadKilled)
    , SomeException
    , evaluate
    , fromException
    , mask
    , throwIO
    , try
    )
import Data.Int (Int64)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import Control.Monad (void)
import Data.Text (Text)
import Dhall.Core (Expr)
import Dhall.Pretty.Internal (layout)
import GHC.Conc (enableAllocationLimit, setAllocationCounter)
import Prettyprinter (Doc, Pretty)

import qualified Data.Text as Text
import qualified Dhall.Core as Core
import qualified Dhall.Eval as Eval
import qualified Prettyprinter as Pretty

-- | Whether a bounded computation produced a whole result.
data Bound a
    = Complete a
    | Truncated a
    | LimitExceeded
    deriving (Eq, Show)

-- | Limits for one bounded normalization.
--
--   'boundOutputBytes' is the size the user asked for.  The allocation limit
--   and the timeout stop evaluation that would otherwise build a value far
--   larger than that.
data BoundLimits = BoundLimits
    { boundOutputBytes :: !Int
    , boundAllocationBytes :: !Int64
    , boundTimeoutMicros :: !Int
    }

-- | 128KiB.  Used when a cap is requested without an explicit size.
defaultOutputBytes :: Int
defaultOutputBytes = 128 * 1024

-- | Output cap of 'defaultOutputBytes', 256MiB of allocation, 30 seconds.
defaultBoundLimits :: BoundLimits
defaultBoundLimits = BoundLimits
    { boundOutputBytes = defaultOutputBytes
    , boundAllocationBytes = 256 * 1024 * 1024
    , boundTimeoutMicros = 30 * 1000 * 1000
    }

-- | Normalize @expression@, stopping when the output budget or the work
--   bound is exhausted.
--
--   'Complete' is a full normal form.  'Truncated' is a partial normal form
--   ending in @…@, or a full form whose rendering does not fit.  'LimitExceeded'
--   means evaluation hit the allocation limit or the timeout; there is no
--   partial value in that case.
normalizeBounded
    :: (Eq a, Pretty a)
    => BoundLimits
    -> Expr s a
    -> IO (Bound (Expr t a))
normalizeBounded limits expression = do
    outcome <- runLimited (boundAllocationBytes limits) (boundTimeoutMicros limits) $ do
        let denoted = Core.denote expression
            value = Eval.eval Eval.Empty denoted
            (quoted, cut) =
                Eval.quoteBounded
                    (boundOutputBytes limits)
                    Eval.EmptyNames
                    value
            result = Core.renote quoted
        evaluate (result, cut)
    case outcome of
        Nothing -> return LimitExceeded
        Just (result, True) -> return (Truncated result)
        Just (result, False) ->
            case prettyBounded (boundOutputBytes limits) (Pretty.pretty result) of
                Complete _ -> return (Complete result)
                Truncated _ -> return (Truncated result)
                LimitExceeded -> return LimitExceeded

-- | Render a document, stopping after @maxBytes@ characters.
--
--   A truncated rendering ends with @…@.
prettyBounded :: Int -> Doc ann -> Bound Text
prettyBounded maxBytes doc
    | maxBytes <= 0 = Truncated "…"
    | otherwise =
        let stream = layout doc
            (text, cut) = takeStream maxBytes stream
        in if cut then Truncated text else Complete text

takeStream :: Int -> Pretty.SimpleDocStream ann -> (Text, Bool)
takeStream budget stream = go budget stream []
  where
    go _ Pretty.SFail acc = (Text.concat (reverse acc), True)
    go _ Pretty.SEmpty acc = (Text.concat (reverse acc), False)
    go 0 _ acc = (Text.concat (reverse ("…" : acc)), True)
    go n (Pretty.SChar c rest) acc =
        go (n - 1) rest (Text.singleton c : acc)
    go n (Pretty.SText len txt rest) acc
        | len < n = go (n - len) rest (txt : acc)
        | otherwise =
            ( Text.concat (reverse (Text.take n txt : "…" : acc))
            , True
            )
    go n (Pretty.SLine indent rest) acc =
        let newline = "\n" <> Text.replicate indent " "
            width = 1 + indent
        in if width >= n
            then (Text.concat (reverse ("…" : acc)), True)
            else go (n - width) rest (newline : acc)
    go n (Pretty.SAnnPush _ rest) acc = go n rest acc
    go n (Pretty.SAnnPop rest) acc = go n rest acc

-- | 'Nothing' means the allocation limit or the timeout fired.
runLimited :: Int64 -> Int -> IO a -> IO (Maybe a)
runLimited allocation timeoutMicros action = mask $ \restore -> do
    box <- newEmptyMVar
    worker <- forkIO $ restore $ do
        setAllocationCounter allocation
        enableAllocationLimit
        outcome <- try action
        void $ tryPutMVar box $ case outcome of
            Right value ->
                Got value
            Left (err :: SomeException)
                | Just AllocationLimitExceeded <- fromException err ->
                    Limited
                | Just ThreadKilled <- fromException err ->
                    Limited
                | otherwise ->
                    Crash err
    watcher <- forkIO $ restore $ do
        threadDelay timeoutMicros
        void $ tryPutMVar box Limited
        killThread worker
    result <- takeMVar box
    killThread worker
    killThread watcher
    case result of
        Got value -> return (Just value)
        Limited -> return Nothing
        Crash err -> throwIO err

data Box a = Got a | Limited | Crash SomeException
