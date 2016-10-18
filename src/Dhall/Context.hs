{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}

-- | All `Context`-related operations

module Dhall.Context (
    -- * Context
      Context
    , empty
    , insert
    , lookup
    , toList
    ) where

import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (<|))
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)

import qualified Data.Foldable
import qualified Data.Map
import qualified Data.Sequence

{-| A @(Context a)@ associates `Text` labels with values of type @a@

    The `Context` is used for type-checking when @(a = Expr X)@

    * You create a `Context` using `empty` and `insert`
    * You transform a `Context` using `fmap`
    * You consume a `Context` using `lookup` and `toList`

    The difference between a `Context` and a `Map` is that a `Context` lets you
    have multiple occurrences of the same key and you can query for the @n@th
    occurrence of a given key.
-}
newtype Context a = Context { getContext :: Map Text (Seq a) }
    deriving (Functor)

-- | An empty context with no key-value pairs
empty :: Context a
empty = Context Data.Map.empty

-- | Add a key-value pair to the `Context`
insert :: Text -> a -> Context a -> Context a
insert k v (Context kvs) =
    Context (Data.Map.insertWith (<>) k (Data.Sequence.singleton v) kvs)
{-# INLINABLE insert #-}

{-| Look up a key by name and index

> lookup _ _         empty  = Nothing
> lookup k 0 (insert k v c) = Just v
> lookup k n (insert k _ c) = lookup k (n - 1) c  -- 1 <= n
> lookup k n (insert j _ c) = lookup k  n      c  -- k /= j
-}
lookup :: Text -> Integer -> Context a -> Maybe a
lookup k n (Context kvs) = do
    vs <- Data.Map.lookup k kvs
    lookupSeq (fromIntegral n) vs
  where
    -- Replace this `Data.Sequence.lookup` once that's available on Stackage LTS
    lookupSeq :: Int -> Seq a -> Maybe a
    lookupSeq n s =
         if n < Data.Sequence.length s
             then Just (Data.Sequence.index s n)
             else Nothing
{-# INLINABLE lookup #-}

{-| Return all key-value associations as a list

> toList           empty  = []
> toList (insert k v ctx) = (k, v) : toList ctx
-}
toList :: Context a -> [(Text, a)]
toList (Context kvs) = do
    (k, vs) <- Data.Map.toList kvs
    v       <- Data.Foldable.toList vs
    return (k, v)
{-# INLINABLE toList #-}
