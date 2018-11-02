{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- This module only exports ways of constructing an OSet,
-- retrieving List, Set, and Seq representations of the same data,
-- as well as a novel "difference" function.
-- Any other Set-like or List-like functionality
-- should be obtained through toSet and toList, respectively.
module Dhall.OSet (
      OSet(..)
    , toList
    , toSet
    , toSeq
    , fromList
    , append
    , empty
    , difference
    ) where

import Prelude
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import Data.Set (Set)
import Data.Data (Data)
import GHC.Generics (Generic)

import qualified Data.Set
import qualified Data.Sequence
import qualified Data.Foldable

data OSet a = OSet (Set a) (Seq a)
  deriving (Eq, Generic, Ord, Show, Data)

toSet :: OSet a -> Set a
toSet (OSet s _) = s

toSeq :: OSet a -> Seq a
toSeq (OSet _ xs) = xs

toList :: OSet a -> [a]
toList = Data.Foldable.toList

instance Foldable OSet where
  foldMap f = foldMap f . toSeq

-- O(n log n) time complexity, O(n) space complexity
-- Implementing it this way is a little silly, but is faster than (nub xs)
-- n.b. oSetAsList . fromList = id, only if the list elements are unique
fromList :: Ord a => [a] -> OSet a
fromList = foldl' (flip append) empty

append :: Ord a => a -> OSet a -> OSet a
append x os@(OSet s xs)
    | Data.Set.member x s = os
    | otherwise = OSet (Data.Set.insert x s) (xs |> x)

empty :: OSet a
empty = OSet Data.Set.empty Data.Sequence.empty

-- | Returns, in order, all elements of the first OSet not present in the second
-- (It doesn't matter in what order the elements appear in the second OSet.)
difference :: Ord a => OSet a -> OSet a -> [a]
difference os (OSet s _) =
    filter (\ x -> not (Data.Set.member x s)) (toList os)
