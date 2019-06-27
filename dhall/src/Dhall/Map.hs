{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , unorderedTraverseWithKey_
    , foldMapWithKey

      -- * Conversions
    , toList
    , toMap
    , keys
    ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.Semigroup
import Prelude hiding (filter, lookup)

import qualified Data.Map
import qualified Data.Set
import qualified GHC.Exts
import qualified Prelude

{-| A `Map` that remembers the original ordering of keys

    This is primarily used so that formatting preserves field order

    This is done primarily to avoid a dependency on @insert-ordered-containers@
    and also to improve performance
-}
data Map k v = Map (Data.Map.Map k v) [k]
    deriving (Data)

instance (Eq k, Eq v) => Eq (Map k v) where
  Map m1 ks == Map m2 ks' = m1 == m2 && ks == ks'
  {-# INLINABLE (==) #-}

instance (Ord k, Ord v) => Ord (Map k v) where
  compare (Map mL ksL) (Map mR ksR) = compare mL mR <> compare ksL ksR

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
    {-# INLINABLE (<>) #-}

instance Ord k => Monoid (Map k v) where
    mempty = Map Data.Map.empty []
    {-# INLINABLE mempty #-}

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
    {-# INLINABLE mappend #-}
#endif

instance (Show k, Show v, Ord k) => Show (Map k v) where
    showsPrec d m =
        showParen (d > 10) (showString "fromList " . showsPrec 11 kvs)
      where
        kvs = toList m

instance Ord k => GHC.Exts.IsList (Map k v) where
    type Item (Map k v) = (k, v)

    fromList = Dhall.Map.fromList

    toList = Dhall.Map.toList

{-| Create a `Map` from a single key-value pair

>>> singleton "A" 1
fromList [("A",1)]
-}
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

>>> fromList [("B",1),("A",2)]  -- The map preserves order
fromList [("B",1),("A",2)]
>>> fromList [("A",1),("A",2)]  -- For duplicates, later values take precedence
fromList [("A",2)]
-}
fromList :: Ord k => [(k, v)] -> Map k v
fromList kvs = Map m ks
  where
    m = Data.Map.fromList kvs

    ks = nubOrd (map fst kvs)
{-# INLINABLE fromList #-}

{-| Remove duplicates from a  list

>>> nubOrd [1,2,3]
[1,2,3]
>>> nubOrd [1,1,3]
[1,3]
-}
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

>>> sort (fromList [("B",1),("A",2)])
fromList [("A",2),("B",1)]
-}
sort :: Ord k => Map k v -> Map k v
sort (Map m _) = Map m ks
  where
    ks = Data.Map.keys m
{-# INLINABLE sort #-}

{-| Check if the keys of a `Map` are already sorted

> isSorted (sort m) = True

>>> isSorted (fromList [("B",1),("A",2)])  -- Sortedness is based only on keys
False
>>> isSorted (fromList [("A",2),("B",1)])
True
-}
isSorted :: Eq k => Map k v -> Bool
isSorted (Map m k) = Data.Map.keys m == k
{-# INLINABLE isSorted #-}

{-| Insert a key-value pair into a `Map`, overriding any previous value stored
    underneath the same key, if present

> insert = insertWith (\v _ -> v)

>>> insert "C" 1 (fromList [("B",2),("A",3)])  -- Values are inserted on left
fromList [("C",1),("B",2),("A",3)]
>>> insert "C" 1 (fromList [("C",2),("A",3)])  -- New value takes precedence
fromList [("C",1),("A",3)]
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

>>> insertWith (+) "C" 1 (fromList [("B",2),("A",3)])  -- No collision
fromList [("C",1),("B",2),("A",3)]
>>> insertWith (+) "C" 1 (fromList [("C",2),("A",3)])  -- Collision
fromList [("C",3),("A",3)]
-}
insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map m ks) = Map m' ks'
  where
    m' = Data.Map.insertWith f k v m

    ks' | elem k ks = ks
        | otherwise = k : ks
{-# INLINABLE insertWith #-}

{-| Delete a key from a `Map` if present, otherwise return the original `Map`

>>> delete "B" (fromList [("C",1),("B",2),("A",3)])
fromList [("C",1),("A",3)]
>>> delete "D" (fromList [("C",1),("B",2),("A",3)])
fromList [("C",1),("B",2),("A",3)]
-}
delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m ks) = Map m' ks'
  where
    m' = Data.Map.delete k m

    ks' = Prelude.filter (k /=) ks
{-# INLINABLE delete #-}

{-| Keep all values that satisfy the given predicate

>>> filter even (fromList [("C",3),("B",2),("A",1)])
fromList [("B",2)]
>>> filter odd (fromList [("C",3),("B",2),("A",1)])
fromList [("C",3),("A",1)]
-}
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter predicate (Map m ks) = Map m' ks'
  where
    m' = Data.Map.filter predicate m

    set = Data.Map.keysSet m'

    ks' = Prelude.filter (\k -> Data.Set.member k set) ks
{-# INLINABLE filter #-}

{-| Transform all values in a `Map` using the supplied function, deleting the
    key if the function returns `Nothing`

>>> mapMaybe Data.Maybe.listToMaybe (fromList [("C",[1]),("B",[]),("A",[3])])
fromList [("C",1),("A",3)]
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

>>> lookup "A" (fromList [("B",1),("A",2)])
Just 2
>>> lookup "C" (fromList [("B",1),("A",2)])
Nothing
-}
lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m _) = Data.Map.lookup k m
{-# INLINABLE lookup #-}

{-| Retrieve the first key, value of the 'Map', if present,
    and also returning the rest of the 'Map'.

> uncons mempty = empty
>
> uncons (singleton k v) = (k, v, mempty)

>>> uncons (fromList [("C",1),("B",2),("A",3)])
Just ("C",1,fromList [("B",2),("A",3)])
>>> uncons (fromList [])
Nothing
-}
uncons :: Ord k => Map k v -> Maybe (k, v, Map k v)
uncons (Map _ [])     = Nothing
uncons (Map m (k:ks)) = Just (k, m Data.Map.! k, Map (Data.Map.delete k m) ks)
{-# INLINABLE uncons #-}

{-| Check if a key belongs to a `Map`

> member k mempty = False
>
> member k (x <> y) = member k x || member k y

>>> member "A" (fromList [("B",1),("A",2)])
True
>>> member "C" (fromList [("B",1),("A",2)])
False
-}
member :: Ord k => k -> Map k v -> Bool
member k (Map m _) = Data.Map.member k m
{-# INLINABLE member #-}

{-| Combine two `Map`s, preferring keys from the first `Map`

> union = unionWith (\v _ -> v)

>>> union (fromList [("D",1),("C",2)]) (fromList [("B",3),("A",4)])
fromList [("D",1),("C",2),("B",3),("A",4)]
>>> union (fromList [("D",1),("C",2)]) (fromList [("C",3),("A",4)])
fromList [("D",1),("C",2),("A",4)]
-}
union :: Ord k => Map k v -> Map k v -> Map k v
union (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.union mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> Prelude.filter (\k -> Data.Set.notMember k setL) ksR
{-# INLINABLE union #-}

{-| Combine two `Map`s using a combining function for colliding keys

>>> unionWith (+) (fromList [("D",1),("C",2)]) (fromList [("B",3),("A",4)])
fromList [("D",1),("C",2),("B",3),("A",4)]
>>> unionWith (+) (fromList [("D",1),("C",2)]) (fromList [("C",3),("A",4)])
fromList [("D",1),("C",5),("A",4)]
-}
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

>>> intersection (fromList [("C",1),("B",2)]) (fromList [("B",3),("A",4)])
fromList [("B",2)]
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

>>> intersectionWith (+) (fromList [("C",1),("B",2)]) (fromList [("B",3),("A",4)])
fromList [("B",5)]
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

>>> difference (fromList [("C",1),("B",2)]) (fromList [("B",3),("A",4)])
fromList [("C",1)]
-}
difference :: Ord k => Map k a -> Map k b -> Map k a
difference (Map mL ksL) (Map mR _) = Map m ks
  where
    m = Data.Map.difference mL mR

    setR = Data.Map.keysSet mR

    ks = Prelude.filter (\k -> Data.Set.notMember k setR) ksL
{-# INLINABLE difference #-}

{-| Fold all of the key-value pairs in a `Map`, in their original order

>>> foldMapWithKey (,) (fromList [("B",[1]),("A",[2])])
("BA",[1,2])
-}
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

>>> mapWithKey (,) (fromList [("B",1),("A",2)])
fromList [("B",("B",1)),("A",("A",2))]
-}
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map m ks) = Map m' ks
  where
    m' = Data.Map.mapWithKey f m
{-# INLINABLE mapWithKey #-}

{-| Traverse all of the key-value pairs in a `Map`, in their original order

>>> traverseWithKey (,) (fromList [("B",1),("A",2)])
("BA",fromList [("B",1),("A",2)])
-}
traverseWithKey
    :: Ord k => Applicative f => (k -> a -> f b) -> Map k a -> f (Map k b)
traverseWithKey f m =
    fmap fromList (traverse f' (toList m))
  where
    f' (k, a) = fmap ((,) k) (f k a)
{-# INLINABLE traverseWithKey #-}

{-| Traverse all of the key-value pairs in a 'Map', not preserving their
    original order, where the result of the computation can be forgotten.
-}
unorderedTraverseWithKey_
    :: Ord k => Applicative f => (k -> a -> f ()) -> Map k a -> f ()
unorderedTraverseWithKey_ f = traverse_ (uncurry f) . toList
{-# INLINABLE unorderedTraverseWithKey_ #-}

{-| Convert a `Map` to a list of key-value pairs in the original order of keys

>>> toList (fromList [("B",1),("A",2)])
[("B",1),("A",2)]
-}
toList :: Ord k => Map k v -> [(k, v)]
toList (Map m ks) = fmap (\k -> (k, m Data.Map.! k)) ks
{-# INLINABLE toList #-}

{-| Convert a @"Dhall.Map".`Map`@ to a @"Data.Map".`Data.Map.Map`@

>>> toMap (fromList [("B",1),("A",2)]) -- Order is lost upon conversion
fromList [("A",2),("B",1)]
-}
toMap :: Map k v -> Data.Map.Map k v
toMap (Map m _) = m
{-# INLINABLE toMap #-}

{-| Return the keys from a `Map` in their original order

>>> keys (fromList [("B",1),("A",2)])
["B","A"]
-}
keys :: Map k v -> [k]
keys (Map _ ks) = ks
{-# INLINABLE keys #-}
