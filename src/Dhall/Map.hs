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
    , filter
    , mapMaybe

      -- * Query
    , lookup
    , member
    , uncons

      -- * Combine
    , union
    , unionWith
    , intersection
    , intersectionWith
    , difference

      -- * Traversals
    , mapWithKey
    , traverseWithKey
    , traverseWithKey_
    , foldMapWithKey

      -- * Conversions
    , toList
    , toMap
    , keys
    ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Semigroup
import Prelude hiding (filter, lookup)

import qualified Data.Map
import qualified Data.Set
import qualified Prelude

{-| A `Map` that remembers the original ordering of keys

    This is primarily used so that formatting preserves field order

    This is done primarily to avoid a dependency on @insert-ordered-containers@
    and also to improve performance
-}
data Map k v = Map (Data.Map.Map k v) [k]
    deriving (Data, Show)

instance (Eq k, Eq v) => Eq (Map k v) where
  (Map m1 ks) == (Map m2 ks') = m1 == m2 && ks == ks'
  {-# INLINABLE (==) #-}

instance Functor (Map k) where
  fmap f (Map m ks) = Map (fmap f m) ks
  {-# INLINABLE fmap #-}

instance Foldable (Map k) where
  foldr f z (Map m _) = foldr f z m
  {-# INLINABLE foldr #-}

  foldMap f (Map m _) = foldMap f m
  {-# INLINABLE foldMap #-}

instance Traversable (Map k) where
  traverse f (Map m ks) = (\m' -> Map m' ks) <$> traverse f m
  {-# INLINABLE traverse #-}

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
{-# INLINABLE singleton #-}

{-| Create a `Map` from a list of key-value pairs

> fromList empty = mempty
>
> fromList (x <|> y) = fromList x <> fromList y
-}
fromList :: Ord k => [(k, v)] -> Map k v
fromList kvs = Map m ks
  where
    m = Data.Map.fromList kvs

    ks = nubOrd (map fst kvs)
{-# INLINABLE fromList #-}

nubOrd :: Ord k => [k] -> [k]
nubOrd = go Data.Set.empty
  where
    go _      []  = []
    go set (k:ks)
        | Data.Set.member k set =     go                    set  ks
        | otherwise             = k : go (Data.Set.insert k set) ks
{-# INLINABLE nubOrd #-}

{-| Sort the keys of a `Map`, forgetting the original ordering

> sort (sort x) = sort x
-}
sort :: Ord k => Map k v -> Map k v
sort (Map m _) = Map m ks
  where
    ks = Data.Map.keys m
{-# INLINABLE sort #-}

{-| Check if the keys of a `Map` are already sorted

> isSorted (sort m) = True
-}
isSorted :: Eq k => Map k v -> Bool
isSorted (Map m k) = Data.Map.keys m == k
{-# INLINABLE isSorted #-}

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
{-# INLINABLE insert #-}

{-| Insert a key-value pair into a `Map`, using the supplied function to combine
    the new value with any old value underneath the same key, if present
-}
insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map m ks) = Map m' ks'
  where
    m' = Data.Map.insertWith f k v m

    ks' | elem k ks = ks
        | otherwise = k : ks
{-# INLINABLE insertWith #-}

-- | Delete a key from a `Map` if present, otherwise return the original `Map`
delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m ks) = Map m' ks'
  where
    m' = Data.Map.delete k m

    ks' = Prelude.filter (k /=) ks
{-# INLINABLE delete #-}

-- | Keep all values that satisfy the given predicate
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter predicate (Map m ks) = Map m' ks'
  where
    m' = Data.Map.filter predicate m

    set = Data.Map.keysSet m'

    ks' = Prelude.filter (\k -> Data.Set.member k set) ks
{-# INLINABLE filter #-}

{-| Transform all values in a `Map` using the supplied function, deleting the
    key if the function returns `Nothing`
-}
mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f (Map m ks) = Map m' ks'
  where
    m' = Data.Map.mapMaybe f m

    set = Data.Map.keysSet m'

    ks' = Prelude.filter (\k -> Data.Set.member k set) ks
{-# INLINABLE mapMaybe #-}

{-| Retrieve a key from a `Map`

> lookup k mempty = empty
>
> lookup k (x <> y) = lookup k y <|> lookup k x
-}
lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m _) = Data.Map.lookup k m
{-# INLINABLE lookup #-}

{-| Retrieve the first key, value of the 'Map', if present,
    and also returning the rest of the 'Map'.

> uncons mempty = empty
>
> uncons (singleton k v) = (k, v, mempty)
-}
uncons :: Ord k => Map k v -> Maybe (k, v, Map k v)
uncons (Map _ [])     = Nothing
uncons (Map m (k:ks)) = Just (k, m Data.Map.! k, Map (Data.Map.delete k m) ks)
{-# INLINABLE uncons #-}

{-| Check if a key belongs to a `Map`

> member k mempty = False
>
> member k (x <> y) = member k x || member k y
-}
member :: Ord k => k -> Map k v -> Bool
member k (Map m _) = Data.Map.member k m
{-# INLINABLE member #-}

{-| Combine two `Map`s, preferring keys from the first `Map`

> union = unionWith (\v _ -> v)
-}
union :: Ord k => Map k v -> Map k v -> Map k v
union (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.union mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> Prelude.filter (\k -> Data.Set.notMember k setL) ksR
{-# INLINABLE union #-}

-- | Combine two `Map`s using a combining function for colliding keys
unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith combine (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.unionWith combine mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> Prelude.filter (\k -> Data.Set.notMember k setL) ksR
{-# INLINABLE unionWith #-}

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
    ks   = Prelude.filter (\k -> Data.Set.member k set) ksL
{-# INLINABLE intersection #-}

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
    ks   = Prelude.filter (\k -> Data.Set.member k set) ksL
{-# INLINABLE intersectionWith #-}

{-| Compute the difference of two `Map`s by subtracting all keys from the
    second `Map` from the first `Map`
-}
difference :: Ord k => Map k a -> Map k b -> Map k a
difference (Map mL ksL) (Map mR _) = Map m ks
  where
    m = Data.Map.difference mL mR

    setR = Data.Map.keysSet mR

    ks = Prelude.filter (\k -> Data.Set.notMember k setR) ksL
{-# INLINABLE difference #-}

-- | Fold all of the key-value pairs in a `Map`, in their original order
foldMapWithKey :: (Monoid m, Ord k) => (k -> a -> m) -> Map k a -> m
foldMapWithKey f m = foldMap (uncurry f) (toList m)
{-# INLINABLE foldMapWithKey #-}

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
{-# INLINABLE mapWithKey #-}

-- | Traverse all of the key-value pairs in a `Map`, in their original order
traverseWithKey
    :: Ord k => Applicative f => (k -> a -> f b) -> Map k a -> f (Map k b)
traverseWithKey f m = fmap fromList (traverse f' (toList m))
  where
    f' (k, a) = fmap ((,) k) (f k a)
{-# INLINABLE traverseWithKey #-}

-- | Traverse all of the key-value pairs in a `Map`, in their original order
--   where the result of the computation can be forgotten.
traverseWithKey_
    :: Ord k => Applicative f => (k -> a -> f ()) -> Map k a -> f ()
traverseWithKey_ f (Map m _) = fmap (const ()) $ Data.Map.traverseWithKey f m
{-# INLINABLE traverseWithKey_ #-}

-- | Convert a `Map` to a list of key-value pairs in the original order of keys
toList :: Ord k => Map k v -> [(k, v)]
toList (Map m ks) = fmap (\k -> (k, m Data.Map.! k)) ks
{-# INLINABLE toList #-}

-- | Convert a @"Dhall.Map".`Map`@ to a @"Data.Map".`Data.Map.Map`@
toMap :: Map k v -> Data.Map.Map k v
toMap (Map m _) = m
{-# INLINABLE toMap #-}

-- | Return the keys from a `Map` in their original order
keys :: Map k v -> [k]
keys (Map _ ks) = ks
{-# INLINABLE keys #-}
