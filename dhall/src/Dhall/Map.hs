{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
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
    , fromListWithKey

      -- * Constructing unordered 'Map's
    , unorderedSingleton
    , unorderedFromList

      -- * Sorting
    , sort
    , isSorted

      -- * Insertion
    , insert
    , insertWith

      -- * Deletion/Update
    , delete
    , filter
    , restrictKeys
    , mapMaybe

      -- * Query
    , lookup
    , member
    , uncons
    , size

      -- * Combine
    , union
    , unionWith
    , outerJoin
    , intersection
    , intersectionWith
    , difference

      -- * Traversals
    , mapWithKey
    , traverseWithKey
    , unorderedTraverseWithKey
    , unorderedTraverseWithKey_
    , foldMapWithKey

      -- * Conversions
    , toList
    , toMap
    , keys
    , keysSet
    , elems
    ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Semigroup
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (filter, lookup)

import qualified Data.List
import qualified Data.Map
import qualified Data.Set
import qualified GHC.Exts
import qualified Prelude

{-| A `Map` that remembers the original ordering of keys

    This is primarily used so that formatting preserves field order

    This is done primarily to avoid a dependency on @insert-ordered-containers@
    and also to improve performance
-}
data Map k v = Map (Data.Map.Map k v) (Keys k)
    deriving (Data, Generic, NFData)

instance (Data k, Data v, Lift k, Lift v, Ord k) => Lift (Map k v)

data Keys a
    = Sorted
    | Original [a]
    deriving (Data, Generic, NFData)

instance (Data a, Lift a) => Lift (Keys a)

instance (Ord k, Eq v) => Eq (Map k v) where
  m1 == m2 =
      Data.Map.size (toMap m1) == Data.Map.size (toMap m2)
      && toList m1 == toList m2
  {-# INLINABLE (==) #-}

{-|
>>> fromList [("A",1),("B",2)] < fromList [("B",1),("A",0)]
True
-}
instance (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toList m1) (toList m2)
  {-# INLINABLE compare #-}

instance Functor (Map k) where
  fmap f (Map m ks) = Map (fmap f m) ks
  {-# INLINABLE fmap #-}

instance Ord k => Foldable (Map k) where
  foldr f z (Map m Sorted) = foldr f z m
  foldr f z m              = foldr f z (elems m)
  {-# INLINABLE foldr #-}

  length m = size m
  {-# INLINABLE length #-}

instance Ord k => Traversable (Map k) where
  traverse f m = traverseWithKey (\_ v -> f v) m
  {-# INLINABLE traverse #-}

{-|
prop> \x y z -> x <> (y <> z) == (x <> y) <> (z :: Map Int Int)
-}
instance Ord k => Data.Semigroup.Semigroup (Map k v) where
    (<>) = union
    {-# INLINABLE (<>) #-}

{-|
prop> \x -> x <> mempty == (x :: Map Int Int)
prop> \x -> mempty <> x == (x :: Map Int Int)
-}
instance Ord k => Monoid (Map k v) where
    mempty = Map Data.Map.empty (Original [])
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

    ks = Original [k]
{-# INLINABLE singleton #-}

{-| Create a `Map` from a list of key-value pairs

>>> fromList [("B",1),("A",2)]  -- The map preserves order
fromList [("B",1),("A",2)]
>>> fromList [("A",1),("A",2)]  -- For duplicates, later values take precedence
fromList [("A",2)]

Note that this handling of duplicates means that 'fromList' is /not/ a monoid
homomorphism:

>>> fromList [(1, True)] <> fromList [(1, False)]
fromList [(1,True)]
>>> fromList ([(1, True)] <> [(1, False)])
fromList [(1,False)]

-}
fromList :: Ord k => [(k, v)] -> Map k v
fromList kvs = Map m ks
  where
    m = Data.Map.fromList kvs

    ks = Original (nubOrd (map fst kvs))
{-# INLINABLE fromList #-}

{-| Create a `Map` from a list of key-value pairs with a combining function.

>>> fromListWithKey (\k v1 v2 -> k ++ v1 ++ v2) [("B","v1"),("A","v2"),("B","v3")]
fromList [("B","Bv3v1"),("A","v2")]
-}
fromListWithKey :: Ord k => (k -> v -> v -> v) -> [(k, v)] -> Map k v
fromListWithKey f kvs = Map m ks
  where
    m = Data.Map.fromListWithKey f kvs

    ks = Original (nubOrd (map fst kvs))
{-# INLINABLE fromListWithKey #-}

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

{-| Create a `Map` from a single key-value pair.

    Any further operations on this map will not retain the order of the keys.

>>> unorderedSingleton "A" 1
fromList [("A",1)]
-}
unorderedSingleton :: k -> v -> Map k v
unorderedSingleton k v = Map m Sorted
  where
    m = Data.Map.singleton k v
{-# INLINABLE unorderedSingleton #-}

{-| Create a `Map` from a list of key-value pairs

    Any further operations on this map will not retain the order of the keys.

>>> unorderedFromList []
fromList []
>>> unorderedFromList [("B",1),("A",2)]  -- The map /doesn't/ preserve order
fromList [("A",2),("B",1)]
>>> unorderedFromList [("A",1),("A",2)]  -- For duplicates, later values take precedence
fromList [("A",2)]
-}
unorderedFromList :: Ord k => [(k, v)] -> Map k v
unorderedFromList kvs = Map m Sorted
  where
    m = Data.Map.fromList kvs
{-# INLINABLE unorderedFromList #-}

{-| Sort the keys of a `Map`, forgetting the original ordering

> sort (sort x) = sort x

>>> sort (fromList [("B",1),("A",2)])
fromList [("A",2),("B",1)]
-}
sort :: Map k v -> Map k v
sort (Map m _) = Map m Sorted
{-# INLINABLE sort #-}

{-| Check if the keys of a `Map` are already sorted

> isSorted (sort m) = True

>>> isSorted (fromList [("B",1),("A",2)])  -- Sortedness is based only on keys
False
>>> isSorted (fromList [("A",2),("B",1)])
True
-}
isSorted :: Eq k => Map k v -> Bool
isSorted (Map _ Sorted)        = True
isSorted (Map m (Original ks)) = Data.Map.keys m == ks -- Or shortcut to False here?
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
insert k v (Map m Sorted)        = Map (Data.Map.insert k v m) Sorted
insert k v (Map m (Original ks)) = Map m' (Original ks')
  where
    (mayOldV, m') = Data.Map.insertLookupWithKey (\_k new _old -> new) k v m

    ks' | Just _ <- mayOldV = ks
        | otherwise         = k : ks
{-# INLINABLE insert #-}

{-| Insert a key-value pair into a `Map`, using the supplied function to combine
    the new value with any old value underneath the same key, if present

>>> insertWith (+) "C" 1 (fromList [("B",2),("A",3)])  -- No collision
fromList [("C",1),("B",2),("A",3)]
>>> insertWith (+) "C" 1 (fromList [("C",2),("A",3)])  -- Collision
fromList [("C",3),("A",3)]
-}
insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map m Sorted)        = Map (Data.Map.insertWith f k v m) Sorted
insertWith f k v (Map m (Original ks)) = Map m' (Original ks')
  where
    (mayOldV, m') = Data.Map.insertLookupWithKey (\_k new old -> f new old) k v m

    ks' | Just _ <- mayOldV = ks
        | otherwise         = k : ks
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

    ks' = case ks of
        Sorted        -> Sorted
        Original ks'' -> Original (Data.List.delete k ks'')
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

    ks' = filterKeys (\k -> Data.Map.member k m') ks
{-# INLINABLE filter #-}

{-| Restrict a 'Map' to only those keys found in a @"Data.Set".'Set'@.

>>> restrictKeys (fromList [("A",1),("B",2)]) (Data.Set.fromList ["A"])
fromList [("A",1)]
-}
restrictKeys :: Ord k => Map k a -> Data.Set.Set k -> Map k a
restrictKeys (Map m ks) s = Map m' ks'
  where
#if MIN_VERSION_containers(0,5,8)
    m' = Data.Map.restrictKeys m s
#else
    m' = Data.Map.filterWithKey (\k _ -> Data.Set.member k s) m
#endif

    ks' = filterKeys (\k -> Data.Set.member k s) ks
{-# INLINABLE restrictKeys #-}

{-| Transform all values in a `Map` using the supplied function, deleting the
    key if the function returns `Nothing`

>>> mapMaybe Data.Maybe.listToMaybe (fromList [("C",[1]),("B",[]),("A",[3])])
fromList [("C",1),("A",3)]
-}
mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f (Map m ks) = Map m' ks'
  where
    m' = Data.Map.mapMaybe f m

    ks' = filterKeys (\k -> Data.Map.member k m') ks
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
uncons (Map _ (Original []))     = Nothing
uncons (Map m (Original (k:ks))) =
    Just (k, m Data.Map.! k, Map (Data.Map.delete k m) (Original ks))
uncons (Map m Sorted)
  | Just ((k, v), m') <- Data.Map.minViewWithKey m = Just (k, v, Map m' Sorted)
  | otherwise                                      = Nothing
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

{-|
>>> size (fromList [("A",1)])
1
-}
size :: Map k v -> Int
size (Map m _) = Data.Map.size m
{-# INLINABLE size #-}

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

    ks = case (ksL, ksR) of
        (Original l, Original r) -> Original $
            l <|> Prelude.filter (\k -> Data.Map.notMember k mL) r
        _                        -> Sorted
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

    ks = case (ksL, ksR) of
        (Original l, Original r) -> Original $
            l <|> Prelude.filter (\k -> Data.Map.notMember k mL) r
        _                        -> Sorted
{-# INLINABLE unionWith #-}

{-| A generalised 'unionWith'.

>>> outerJoin Left Left (\k a b -> Right (k, a, b)) (fromList [("A",1),("B",2)]) (singleton "A" 3)
fromList [("A",Right ("A",1,3)),("B",Left 2)]

This function is much inspired by the "Data.Semialign.Semialign" class.
-}
outerJoin
    :: Ord k
    => (a -> c)
    -> (b -> c)
    -> (k -> a -> b -> c)
    -> Map k a
    -> Map k b
    -> Map k c
outerJoin fa fb fab (Map ma ksA) (Map mb ksB) = Map m ks
  where
    m = Data.Map.mergeWithKey
            (\k a b -> Just (fab k a b))
            (fmap fa)
            (fmap fb)
            ma
            mb

    ks = case (ksA, ksB) of
        (Original l, Original r) -> Original $
            l <|> Prelude.filter (\k -> Data.Map.notMember k ma) r
        _                        -> Sorted
{-# INLINABLE outerJoin #-}

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

    -- Or forget order unless both maps are ordered?!
    ks = filterKeys (\k -> Data.Map.member k m) ksL
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

    -- Or forget order unless both maps are ordered?!
    ks = filterKeys (\k -> Data.Map.member k m) ksL
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

    ks = filterKeys (\k -> Data.Map.notMember k mR) ksL
{-# INLINABLE difference #-}

{-| Fold all of the key-value pairs in a `Map`, in their original order

>>> foldMapWithKey (,) (fromList [("B",[1]),("A",[2])])
("BA",[1,2])
-}
foldMapWithKey :: (Monoid m, Ord k) => (k -> a -> m) -> Map k a -> m
foldMapWithKey f (Map m Sorted) = Data.Map.foldMapWithKey f m
foldMapWithKey f m              = foldMap (uncurry f) (toList m)
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
traverseWithKey f (Map m Sorted) =
    fmap (\m' -> Map m' Sorted) (Data.Map.traverseWithKey f m)
traverseWithKey f m =
    fmap fromList (traverse f' (toList m))
  where
    f' (k, a) = fmap ((,) k) (f k a)
{-# INLINABLE traverseWithKey #-}

{-| Same as `traverseWithKey`, except that the order of effects is not
    necessarily the same as the order of the keys
-}
unorderedTraverseWithKey
    :: Ord k => Applicative f => (k -> a -> f b) -> Map k a -> f (Map k b)
unorderedTraverseWithKey f (Map m ks) =
    fmap (\m' -> Map m' ks) (Data.Map.traverseWithKey f m)
{-# INLINABLE unorderedTraverseWithKey #-}

{-| Traverse all of the key-value pairs in a 'Map', not preserving their
    original order, where the result of the computation can be forgotten.

    Note that this is a strict traversal, fully traversing the map even
    when the Applicative is lazy in the remaining elements.
-}
unorderedTraverseWithKey_
    :: Ord k => Applicative f => (k -> a -> f ()) -> Map k a -> f ()
unorderedTraverseWithKey_ f (Map m _) =
    Data.Map.foldlWithKey' (\acc k v -> acc *> f k v) (pure ()) m
{-# INLINABLE unorderedTraverseWithKey_ #-}

{-| Convert a `Map` to a list of key-value pairs in the original order of keys

>>> toList (fromList [("B",1),("A",2)])
[("B",1),("A",2)]
-}
toList :: Ord k => Map k v -> [(k, v)]
toList (Map m Sorted)        = Data.Map.toList m
toList (Map m (Original ks)) = fmap (\k -> (k, m Data.Map.! k)) ks
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
keys (Map m Sorted)        = Data.Map.keys m
keys (Map _ (Original ks)) = ks
{-# INLINABLE keys #-}

{-| Return the values from a `Map` in their original order.

>>> elems (fromList [("B",1),("A",2)])
[1,2]
-}
elems :: Ord k => Map k v -> [v]
elems (Map m Sorted)        = Data.Map.elems m
elems (Map m (Original ks)) = fmap (\k -> m Data.Map.! k) ks
{-# INLINABLE elems #-}

{-| Return the @"Data.Set".'Set'@ of the keys

>>> keysSet (fromList [("B",1),("A",2)])
fromList ["A","B"]
-}
keysSet :: Map k v -> Data.Set.Set k
keysSet (Map m _) = Data.Map.keysSet m
{-# INLINABLE keysSet #-}

filterKeys :: (a -> Bool) -> Keys a -> Keys a
filterKeys _ Sorted        = Sorted
filterKeys f (Original ks) = Original (Prelude.filter f ks)
{-# INLINABLE filterKeys #-}

{- $setup
>>> import Test.QuickCheck (Arbitrary(..), oneof)
>>> :{
  instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = oneof [fromList <$> arbitrary, unorderedFromList <$> arbitrary]
:}
-}
