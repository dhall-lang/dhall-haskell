module Dhall.Kubernetes.Data where

import Dhall.Kubernetes.Types

import qualified Data.Map.Strict as Data.Map
import qualified Data.Set        as Set


-- | This just removes the offending keys from the definition
patchCyclicImports :: Definition -> Definition
patchCyclicImports Definition{ properties = oldProps, .. } = Definition{..}
  where
    properties = fmap (\propsMap -> Data.Map.withoutKeys propsMap toRemove) oldProps
    toRemove =
      Set.fromList $
        (   ModelName
        <$> [ "allOf"
            , "anyOf"
            , "not"
            , "oneOf"
            , "additionalItems"
            , "additionalProperties"
            , "items"
            ]
        )
