{-# LANGUAGE CPP #-}

-- | Compatibility helpers for the @aeson-2@ migration.
module Dhall.JSON.Compat (
      objectFromList
    , mapToAscList
    , filterObject
    , lookupObject
    , traverseObjectWithKey
    , objectKeys
    , textToKey
    ) where

import Data.Aeson (Object, Value)
import Data.Text  (Text)

#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.Key    (Key)
import qualified Data.Aeson.Key    as Key
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Bifunctor    (first)
#else
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
#endif

objectFromList :: [(Text, Value)] -> Object
#if MIN_VERSION_aeson(2,0,0)
objectFromList = KeyMap.fromList . map (first Key.fromText)
#else
objectFromList = HashMap.fromList
#endif

filterObject :: (Value -> Bool) -> Object -> Object
#if MIN_VERSION_aeson(2,0,0)
filterObject = KeyMap.filter
#else
filterObject = HashMap.filter
#endif

#if MIN_VERSION_aeson(2,0,0)
mapToAscList :: KeyMap a -> [(Text, a)]
mapToAscList = map (first Key.toText) . KeyMap.toAscList
#else
mapToAscList :: HashMap Text a -> [(Text, a)]
mapToAscList = List.sortOn fst . HashMap.toList
#endif

lookupObject :: Text -> Object -> Maybe Value
#if MIN_VERSION_aeson(2,0,0)
lookupObject k = KeyMap.lookup (Key.fromText k)
#else
lookupObject = HashMap.lookup
#endif

objectKeys :: Object -> [Text]
#if MIN_VERSION_aeson(2,0,0)
objectKeys = map (Key.toText) . KeyMap.keys
#else
objectKeys = HashMap.keys
#endif

#if MIN_VERSION_aeson(2,0,0)
textToKey :: Text -> Key
textToKey = Key.fromText
#else
textToKey :: Text -> Text
textToKey = id
#endif

#if MIN_VERSION_aeson(2,0,0)
traverseObjectWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseObjectWithKey = KeyMap.traverseWithKey
#else
traverseObjectWithKey :: Applicative f => (Text -> v1 -> f v2) -> HashMap Text v1 -> f (HashMap Text v2)
traverseObjectWithKey = HashMap.traverseWithKey
#endif
