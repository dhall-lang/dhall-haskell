-- | `Map` type used to represent records and unions

module Dhall.Map
    ( Map
    , Data.HashMap.Strict.InsOrd.delete
    , Data.HashMap.Strict.InsOrd.empty
    , Data.HashMap.Strict.InsOrd.fromList
    , Data.HashMap.Strict.InsOrd.foldMapWithKey
    , Data.HashMap.Strict.InsOrd.insert
    , Data.HashMap.Strict.InsOrd.insertWith
    , Data.HashMap.Strict.InsOrd.intersectionWith
    , Data.HashMap.Strict.InsOrd.lookup
    , Data.HashMap.Strict.InsOrd.keys
    , Data.HashMap.Strict.InsOrd.mapWithKey
    , Data.HashMap.Strict.InsOrd.member
    , Data.HashMap.Strict.InsOrd.null
    , Data.HashMap.Strict.InsOrd.singleton
    , Data.HashMap.Strict.InsOrd.toList
    , Data.HashMap.Strict.InsOrd.toHashMap
    , Data.HashMap.Strict.InsOrd.traverseWithKey
    , Data.HashMap.Strict.InsOrd.union
    , Data.HashMap.Strict.InsOrd.unionWith
    ) where

import qualified Data.HashMap.Strict.InsOrd

type Map = Data.HashMap.Strict.InsOrd.InsOrdHashMap
