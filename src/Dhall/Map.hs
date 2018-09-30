{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | `Map` type used to represent records and unions

module Dhall.Map
    ( Map
    , delete
    , empty
    , foldMapWithKey
    , fromList
    , insert
    , insertWith
    , intersectionWith
    , lookup
    , keys
    , mapWithKey
    , member
    , singleton
    , sort
    , isSorted
    , toList
    , toMap
    , traverseWithKey
    , union
    , unionWith
    , Data.Foldable.null
    ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Prelude hiding (lookup)

import qualified Data.Foldable
import qualified Data.Map
import qualified Data.Set

data Map k v = Map (Data.Map.Map k v) [k]
    deriving (Data, Eq, Foldable, Functor, Show, Traversable)

instance Ord k => Semigroup (Map k v) where
    (<>) = union

instance Ord k => Monoid (Map k v) where
    mempty = empty

    mappend = (<>)

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

sort :: Ord k => Map k v -> Map k v
sort (Map m _) = Map m ks
  where
    ks = Data.Map.keys m

isSorted :: Eq k => Map k v -> Bool
isSorted (Map m k) = Data.Map.keys m == k

unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith combine (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.unionWith combine mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> filter (\k -> Data.Set.notMember k setL) ksR

union :: Ord k => Map k v -> Map k v -> Map k v
union (Map mL ksL) (Map mR ksR) = Map m ks
  where
    m = Data.Map.union mL mR

    setL = Data.Map.keysSet mL

    ks = ksL <|> filter (\k -> Data.Set.notMember k setL) ksR

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m _) = Data.Map.lookup k m

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map m ks) = Map m' ks
  where
    m' = Data.Map.mapWithKey f m

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m ks) = Map m' ks'
  where
    m' = Data.Map.delete k m

    ks' = filter (k /=) ks

keys :: Map k v -> [k]
keys (Map _ ks) = ks

member :: Ord k => k -> Map k v -> Bool
member k (Map m _) = Data.Map.member k m

toList :: Ord k => Map k v -> [(k, v)]
toList (Map m ks) = fmap (\k -> (k, m Data.Map.! k)) ks

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith combine (Map mL ksL) (Map mR _) = Map m ks
  where
    m = Data.Map.intersectionWith combine mL mR

    setL = Data.Map.keysSet mL
    setR = Data.Map.keysSet mR
    set  = Data.Set.intersection setL setR
    ks   = filter (\k -> Data.Set.member k set) ksL

foldMapWithKey :: (Monoid m, Ord k) => (k -> a -> m) -> Map k a -> m
foldMapWithKey f m = foldMap (uncurry f) (toList m)

empty :: Map k a
empty = Map Data.Map.empty []

traverseWithKey
    :: Ord k => Applicative f => (k -> a -> f b) -> Map k a -> f (Map k b)
traverseWithKey f m = fmap fromList (traverse f' (toList m))
  where
    f' (k, a) = fmap ((,) k) (f k a)

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m ks) = Map m' ks'
  where
    m' = Data.Map.insert k v m

    ks' | elem k ks = ks
        | otherwise = k : ks

insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map m ks) = Map m' ks'
  where
    m' = Data.Map.insertWith f k v m

    ks' | elem k ks = ks
        | otherwise = k : ks

singleton :: k -> v -> Map k v
singleton k v = Map m ks
  where
    m = Data.Map.singleton k v

    ks = pure k

toMap :: Map k v -> Data.Map.Map k v
toMap (Map m _) = m
