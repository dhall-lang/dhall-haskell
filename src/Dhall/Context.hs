{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}

-- | This is a utility module that consolidates all `Context`-related operations

module Dhall.Context (
    -- * Context
      Context
    , empty
    , insert
    , match
    , lookup
    , toList
    ) where

import Data.Text.Lazy (Text)
import Prelude hiding (lookup)

{-| A @(Context a)@ associates `Text` labels with values of type @a@.  Each
    `Text` label can correspond to multiple values of type @a@

    The `Context` is used for type-checking when @(a = Expr X)@

    * You create a `Context` using `empty` and `insert`
    * You transform a `Context` using `fmap`
    * You consume a `Context` using `lookup` and `toList`

    The difference between a `Context` and a `Data.Map.Map` is that a `Context`
    lets you have multiple ordered occurrences of the same key and you can
    query for the @n@th occurrence of a given key.
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

{-| \"Pattern match\" on a `Context`

> match (insert k v ctx) = Just (k, v, ctx)
> match  empty           = Nothing
-}
match :: Context a -> Maybe (Text, a, Context a)
match (Context ((k, v) : kvs)) = Just (k, v, Context kvs)
match (Context           []  ) = Nothing

{-# INLINABLE match #-}

{-| Look up a key by name and index

> lookup _ _         empty  = Nothing
> lookup k 0 (insert k v c) = Just v
> lookup k n (insert k v c) = lookup k (n - 1) c
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
