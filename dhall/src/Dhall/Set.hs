{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module only exports ways of constructing a Set,
-- retrieving List, Set, and Seq representations of the same data,
-- as well as a novel "difference" function.
-- Any other Set-like or List-like functionality
-- should be obtained through toSet and toList, respectively.
module Dhall.Set (
      Set(..)
    , toList
    , toSet
    , toSeq
    , fromList
    , fromSet
    , append
    , empty
    , difference
    , sort
    , isSorted
    ) where

import Prelude
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import Data.Data (Data)
import GHC.Generics (Generic)

import qualified Data.Set
import qualified Data.Sequence
import qualified Data.Foldable

data Set a = Set (Data.Set.Set a) (Seq a)
    deriving (Eq, Generic, Ord, Show, Data)

instance Foldable Set where
    foldMap f = foldMap f . toSeq

toSet :: Set a -> Data.Set.Set a
toSet (Set s _) = s

toSeq :: Set a -> Seq a
toSeq (Set _ xs) = xs

toList :: Set a -> [a]
toList = Data.Foldable.toList

-- O(n log n) time complexity, O(n) space complexity.
-- Implementing it this way is a little silly, but is faster than (nub xs).
-- n.b. toList . fromList = id, only if the list elements are unique
fromList :: Ord a => [a] -> Set a
fromList = foldl' (flip append) empty

fromSet :: Data.Set.Set a -> Set a
fromSet s = Set s (Data.Sequence.fromList (Data.Set.elems s))

-- O(log n) time complexity.
append :: Ord a => a -> Set a -> Set a
append x os@(Set s xs)
    | Data.Set.member x s = os
    | otherwise = Set (Data.Set.insert x s) (xs |> x)

empty :: Set a
empty = Set Data.Set.empty Data.Sequence.empty

-- | Returns, in order, all elements of the first Set not present in the second.
-- (It doesn't matter in what order the elements appear in the second Set.)
difference :: Ord a => Set a -> Set a -> [a]
difference os (Set s _) =
    filter (\ x -> not (Data.Set.member x s)) (toList os)

{-| Sort the set elements, forgetting their original ordering.

>>> sort (fromList [2, 1]) == fromList [1, 2]
True
-}
sort :: Ord a => Set a -> Set a
sort (Set s xs) = Set s (Data.Sequence.sort xs)

{-|
>>> isSorted (fromList [2, 1])
False
>>> isSorted (fromList [1, 2])
True
-}
isSorted :: Ord a => Set a -> Bool
isSorted s = toList s == Data.Set.toList (toSet s)
