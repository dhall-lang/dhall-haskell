{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | `Map` type used to represent records and unions

module Dhall.Map
    ( -- * Type
      Map

      -- * Construction
    , singleton
    , fromList

      -- * Sorting
    , sort
    , isSorted

      -- * Insertion
    , insert
    , insertWith

      -- * Deletion/Update
    , delete

      -- * Query
    , lookup
    , member

      -- * Combine
    , union
    , unionWith
    , intersection
    , intersectionWith

      -- * Traversals
    , mapWithKey
    , traverseWithKey
    , foldMapWithKey

      -- * Conversions
    , toList
    , toMap
    , keys
    ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Semigroup
import Prelude hiding (lookup)

import qualified Data.Map
import qualified Data.Set

{-| A `Map` that remembers the original ordering of keys

    This is primarily used so that formatting preserves field order

    This is done primarily to avoid a dependency on @insert-ordered-containers@
    and also to improve performance
-}
data Map k v = Map (Data.Map.Map k v) [k]
    deriving (Data, Eq, Foldable, Functor, Show, Traversable)

instance Ord k => Data.Semigroup.Semigroup (Map k v) where
    (<>) = union

instance Ord k => Monoid (Map k v) where
    mempty = Map Data.Map.empty []

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- | Create a `Map` from a single key-value pair
singleton :: k -> v -> Map k v
singleton k v = Map m ks
  where
    m = Data.Map.singleton k v

    ks = pure k

{-| Create a `Map` from a list of key-value pairs

> fromList empty = mempty
>
> fromList (x <|> y) = fromList x <> fromList y
-}
fromList :: Ord k => [(k, v)] -> Map k v
fromList kvs = Map m ks
  where
    m = Data.Map.fromList kvs

    ks = nub (map fst kvs)

nub :: Ord k => [k] -> [k]
nub = go Data.Set.empty
  where
    go _      []  = []
    go set (k:ks)
        | Data.Set.member k set =     go                    set  ks
        | otherwise             = k : go (Data.Set.insert k set) ks

{-| Sort the keys of a `Map`, forgetting the original ordering

> sort (sort x) = sort x
-}
sort :: Ord k => Map k v -> Map k v
sort (Map m _) = Map m ks
  where
    ks = Data.Map.keys m

{-| Check if the keys of a `Map` are already sorted

> isSorted (sort m) = True
-}
isSorted :: Eq k => Map k v -> Bool
isSorted (Map m k) = Data.Map.keys m == k

{-| Insert a key-value pair into a `Map`, overriding any previous value stored
    underneath the same key, if present

> insert = insertWith (\v _ -> v)
-}
insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m ks) = Map m' ks'
  where
    m' = Data.Map.insert k v m

    ks' | elem k ks = ks
        | otherwise = k : ks

{-| Insert a key-value pair into a `Map`, using the supplied function to combine
    the new value with any old value underneath the same key, if present
-}
insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map m ks) = Map m' ks'
  where
    m' = Data.Map.insertWith f k v m

    ks' | elem k ks = ks
        | otherwise = k : ks

-- | Delete a key from a `Map` if present, otherwise return the original `Map`
delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m ks) = Map m' ks'
  where
    m' = Data.Map.delete k m

    ks' = filter (k /=) ks

{-| Retrieve a key from a `Map`

> lookup k mempty = empty
>
> lookup k (x <> y) = lookup k y <|> lookup k x
-}
lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m _) = Data.Map.lookup k m

{-| Check if a key belongs to a `Map`

> member k mempty = False
>
> member k (x <> y) = member k x || member k y
-}
member :: Ord k => k -> Map k v -> Bool
member k (Map m _) = Data.Map.member k m

{-| Combine two `Map`s, preferring keys from the first `Map`

> union = unionWith (\v _ -> v)
-}
union :: Ord k => Map k v -> Map k v -> Map k v
union (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.union mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> filter (\k -> Data.Set.notMember k setL) ksR

-- | Combine two `Map`s using a combining function for colliding keys
unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith combine (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.unionWith combine mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> filter (\k -> Data.Set.notMember k setL) ksR

{-| Combine two `Map` on their shared keys, keeping the value from the first
    `Map`

> intersection = intersectionWith (\v _ -> v)
-}
intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection (Map mL ksL) (Map mR _) = Map m ks
  where
    m = Data.Map.intersection mL mR

    setL = Data.Map.keysSet mL
    setR = Data.Map.keysSet mR
    set  = Data.Set.intersection setL setR
    ks   = filter (\k -> Data.Set.member k set) ksL

{-| Combine two `Map`s on their shared keys, using the supplied function to
    combine values from the first and second `Map`
-}
intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith combine (Map mL ksL) (Map mR _) = Map m ks
  where
    m = Data.Map.intersectionWith combine mL mR

    setL = Data.Map.keysSet mL
    setR = Data.Map.keysSet mR
    set  = Data.Set.intersection setL setR
    ks   = filter (\k -> Data.Set.member k set) ksL

-- | Fold all of the key-value pairs in a `Map`, in their original order
foldMapWithKey :: (Monoid m, Ord k) => (k -> a -> m) -> Map k a -> m
foldMapWithKey f m = foldMap (uncurry f) (toList m)

{-| Transform the values of a `Map` using their corresponding key

> mapWithKey (pure id) = id
>
> mapWithKey (liftA2 (.) f g) = mapWithKey f . mapWithKey g

> mapWithKey f mempty = mempty
>
> mapWithKey f (x <> y) = mapWithKey f x <> mapWithKey f y
-}
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map m ks) = Map m' ks
  where
    m' = Data.Map.mapWithKey f m

-- | Traverse all of the key-value pairs in a `Map`, in their original order
traverseWithKey
    :: Ord k => Applicative f => (k -> a -> f b) -> Map k a -> f (Map k b)
traverseWithKey f m = fmap fromList (traverse f' (toList m))
  where
    f' (k, a) = fmap ((,) k) (f k a)

-- | Convert a `Map` to a list of key-value pairs in the original order of keys
toList :: Ord k => Map k v -> [(k, v)]
toList (Map m ks) = fmap (\k -> (k, m Data.Map.! k)) ks

-- | Convert a @"Dhall.Map".`Map`@ to a @"Data.Map".`Data.Map.Map`@
toMap :: Map k v -> Data.Map.Map k v
toMap (Map m _) = m

-- | Return the keys from a `Map` in their original order
keys :: Map k v -> [k]
keys (Map _ ks) = ks
