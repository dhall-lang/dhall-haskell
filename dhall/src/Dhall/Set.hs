{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
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
    , null
    ) where

import Prelude hiding (null)
import Control.DeepSeq (NFData)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import Data.Data (Data)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

import qualified Data.Set
import qualified Data.Sequence
import qualified Data.Foldable

{-| This is a variation on @"Data.Set".`Data.Set.Set`@ that remembers the
    original order of elements.  This ensures that ordering is not lost when
    formatting Dhall code
-}
data Set a = Set (Data.Set.Set a) (Seq a)
    deriving (Generic, Show, Data, NFData)
-- Invariant: In @Set set seq@, @toAscList set == sort (toList seq)@.

instance Eq a => Eq (Set a) where
    (Set _ x) == (Set _ y) = x == y
    {-# INLINABLE (==) #-}

instance Ord a => Ord (Set a) where
    compare (Set _ x) (Set _ y) = compare x y
    {-# INLINABLE compare #-}

instance (Data a, Lift a, Ord a) => Lift (Set a)

instance Foldable Set where
    foldMap f (Set _ x) = foldMap f x
    {-# INLINABLE foldMap #-}

-- | Convert to an unordered @"Data.Set".`Data.Set.Set`@
toSet :: Set a -> Data.Set.Set a
toSet (Set s _) = s

-- | Convert to an ordered `Seq`
toSeq :: Set a -> Seq a
toSeq (Set _ xs) = xs

-- | Convert a `Set` to a list, preserving the original order of the elements
toList :: Set a -> [a]
toList = Data.Foldable.toList

-- | Convert a list to a `Set`, remembering the element order
fromList :: Ord a => [a] -> Set a
fromList = foldl' (flip append) empty
-- O(n log n) time complexity, O(n) space complexity.
-- Implementing it this way is a little silly, but is faster than (nub xs).
-- n.b. toList . fromList = id, only if the list elements are unique

-- | Convert a @"Data.Set".`Data.Set.Set`@ to a sorted `Set`
fromSet :: Data.Set.Set a -> Set a
fromSet s = Set s (Data.Sequence.fromList (Data.Set.elems s))

-- | Append an element to the end of a `Set`
append :: Ord a => a -> Set a -> Set a
append x os@(Set s xs)
    | Data.Set.member x s = os
    | otherwise = Set (Data.Set.insert x s) (xs |> x)
-- O(log n) time complexity.

-- | The empty `Set`
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

{-|
>>> null (fromList [1])
False
>>> null (fromList [])
True
-}
null :: Set a -> Bool
null (Set s _) = Data.Set.null s
