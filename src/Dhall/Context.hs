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

> lookup k  empty            = Nothing
> lookup k (insert k' v ctx) = if k == k' then Just v else lookup k ctx
-}
lookup :: Text -> Context a -> Maybe a
lookup k (Context kvs0) = loop kvs0
  where
    loop ((k', v):kvs)
        | k == k'   = Just v
        | otherwise = loop kvs
    loop  []        = Nothing
{-# INLINABLE lookup #-}

{-| Return all key-value associations as a list

> toList  empty           = []
> toList (insert k v ctx) = (k, v) : toList ctx
-}
toList :: Context a -> [(Text, a)]
toList = getContext
{-# INLINABLE toList #-}
