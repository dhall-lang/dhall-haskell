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

-- | Bound variable names and their types
newtype Context a = Context { getContext :: [(Text, a)] }
    deriving (Functor)

-- | An empty context with no key-value pairs
empty :: Context a
empty = Context []

-- | Add a key-value pair to the `Context`
insert :: Text -> a -> Context a -> Context a
insert k v (Context kvs) = Context ((k, v) : kvs)
{-# INLINABLE insert #-}

{-| Lookup a key by name and index

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

> toList  empty           = []
> toList (insert k v ctx) = (k, v) : toList ctx
-}
toList :: Context a -> [(Text, a)]
toList = getContext
{-# INLINABLE toList #-}
