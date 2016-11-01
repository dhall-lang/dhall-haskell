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

import Data.Text.Lazy (Text)
import Prelude hiding (lookup)

{-| A @(Context a)@ associates `Text` labels with values of type @a@

    The `Context` is used for type-checking when @(a = Expr X)@

    * You create a `Context` using `empty` and `insert`
    * You transform a `Context` using `fmap`
    * You consume a `Context` using `lookup` and `toList`

    The difference between a `Context` and a `Map` is that a `Context` lets you
    have multiple occurrences of the same key and you can query for the @n@th
    occurrence of a given key.
-}
newtype Context a = Context { getContext :: [(Text, a)] }
    deriving (Functor)

-- | An empty context with no key-value pairs
empty :: Context a
empty = Context []

-- | Add a key-value pair to the `Context`
insert :: Text -> a -> Context a -> Context a
insert k v (Context kvs) = Context ((k, v) : kvs)
{-# INLINABLE insert #-}

{-| Look up a key by name and index

> lookup _ _         empty  = Nothing
> lookup k 0 (insert k v c) = Just v
> lookup k n (insert k v c) = lookup k (n - 1) c  -- 1 <= n
> lookup k n (insert j v c) = lookup k  n      c  -- k /= j
-}
lookup :: Text -> Integer -> Context a -> Maybe a
lookup _ !_ (Context         []  ) =
    Nothing
lookup x !n (Context ((k, v):kvs)) =
    if x == k
    then if n == 0
         then Just v
         else lookup x (n - 1) (Context kvs)
    else lookup x n (Context kvs)
{-# INLINABLE lookup #-}

{-| Return all key-value associations as a list

> toList           empty  = []
> toList (insert k v ctx) = (k, v) : toList ctx
-}
toList :: Context a -> [(Text, a)]
toList = getContext
{-# INLINABLE toList #-}
