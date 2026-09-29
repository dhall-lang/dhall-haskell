{-| Pretty-printing and normalization with independent limits.

    The three limits combine freely; each one that is omitted is not applied.

    @--max-output-size@ does two things.  Quoting spends that many bytes,
    charging each syntax node for the text it will print (a little syntax,
    plus the length of names and literals), and stops once the estimate is
    used up.  Rendering then keeps at most that many bytes of the real
    layout and ends a truncated form with @…@.  Quoting does not measure
    indentation, so the render step is what enforces the exact cap.

    @--max-allocation@ and @--max-evaluation-time@ are separate.  They abort
    normalization with no partial normal form.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Bounded
    ( Bound(..)
    , WorkLimit(..)
    , normalizeLimited
    , prettyBounded
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.DeepSeq (NFData, force)
import Control.Exception
    ( AsyncException (ThreadKilled)
    , SomeException
    , evaluate
    , fromException
    , mask
    , throwIO
    , try
    )
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Core (Expr)
import Dhall.Pretty.Internal (layout)
import GHC.Conc (enableAllocationLimit, setAllocationCounter)
import GHC.IO.Exception (AllocationLimitExceeded (..))
import Prettyprinter (Doc)

import qualified Data.Text as Text
import qualified Dhall.Core as Core
import qualified Dhall.Eval as Eval
import qualified Prettyprinter as Pretty

-- | Whether a rendered document fit in the output budget.
data Bound a
    = Complete a
    | Truncated a
    deriving (Eq, Show)

-- | Which work limit stopped normalization.  There is no partial normal form.
data WorkLimit
    = AllocationExceeded
    | TimeExceeded
    deriving (Eq, Show)

-- | Normalize @expression@.
--
--   'Nothing' for a limit means that limit is not applied, so any combination
--   of the three is allowed.  A non-positive allocation or timeout is already
--   exceeded.  When output bytes are given, quoting stops once its estimate
--   of rendered text reaches that size; the 'Bool' is 'True' when quoting
--   was cut short.  Without output bytes, the result is an ordinary normal
--   form and the 'Bool' is 'False'.
normalizeLimited
    :: forall a s t. (Eq a, NFData a)
    => Maybe Int64
    -> Maybe Int
    -> Maybe Int
    -> Expr s a
    -> IO (Either WorkLimit (Expr t a, Bool))
normalizeLimited allocationBytes timeoutMicros outputBytes expression
    | exceeds allocationBytes = return (Left AllocationExceeded)
    | exceeds timeoutMicros = return (Left TimeExceeded)
    | otherwise =
        runLimited allocationBytes timeoutMicros $
            case outputBytes of
                Nothing -> do
                    let normalForm :: Expr Void a
                        normalForm = Core.normalize expression
                    forced <- evaluate (force normalForm)
                    return (Core.renote forced, False)
                Just nbytes -> do
                    let denoted = Core.denote expression
                        value = Eval.eval Eval.Empty denoted
                        (quoted, cut) =
                            Eval.quoteBounded nbytes Eval.EmptyNames value
                    forced <- evaluate (force quoted)
                    return (Core.renote forced, cut)
  where
    exceeds Nothing = False
    exceeds (Just n) = n <= 0

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
    go 0 (Pretty.SAnnPush _ rest) acc = go 0 rest acc
    go 0 (Pretty.SAnnPop rest) acc = go 0 rest acc
    go 0 _ acc = (Text.concat (reverse ("…" : acc)), True)
    go n (Pretty.SChar c rest) acc =
        go (n - 1) rest (Text.singleton c : acc)
    go n (Pretty.SText len txt rest) acc
        | len <= n = go (n - len) rest (txt : acc)
        | otherwise =
            -- @acc@ is stored in reverse.  The fitting prefix comes first in
            -- the rendered text; @…@ is the last thing emitted.
            ( Text.concat (reverse ("…" : Text.take n txt : acc))
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

-- | 'Left' means the allocation limit or the timeout fired.
--
--   'Nothing' for a limit means that limit is not applied.
runLimited :: Maybe Int64 -> Maybe Int -> IO a -> IO (Either WorkLimit a)
runLimited allocation timeoutMicros action = mask $ \restore -> do
    box <- newEmptyMVar
    worker <- forkIO $ restore $ do
        case allocation of
            Nothing -> return ()
            Just bytes -> do
                setAllocationCounter bytes
                enableAllocationLimit
        outcome <- try action
        void $ tryPutMVar box $ case outcome of
            Right value ->
                Got value
            Left (err :: SomeException)
                | Just AllocationLimitExceeded <- fromException err ->
                    Stop AllocationExceeded
                | Just ThreadKilled <- fromException err ->
                    Stop TimeExceeded
                | otherwise ->
                    Crash err
    watcher <- case timeoutMicros of
        Nothing -> return Nothing
        Just micros -> fmap Just $ forkIO $ restore $ do
            threadDelay micros
            void $ tryPutMVar box (Stop TimeExceeded)
            killThread worker
    result <- takeMVar box
    killThread worker
    mapM_ killThread watcher
    case result of
        Got value -> return (Right value)
        Stop reason -> return (Left reason)
        Crash err -> throwIO err

data Box a = Got a | Stop WorkLimit | Crash SomeException
