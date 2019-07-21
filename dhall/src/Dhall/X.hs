{-# LANGUAGE RankNTypes #-}

module Dhall.X where

import Data.Data (Data(..))
import Data.Text.Prettyprint.Doc (Pretty(..))

-- | Like `Data.Void.Void`, except with a shorter inferred type
newtype X = X { absurd :: forall a . a }

instance Show X where
    show = absurd

instance Eq X where
  _ == _ = True

instance Data X where
    dataTypeOf = absurd
    gunfold _ _ _ = undefined
    toConstr = absurd

instance Pretty X where
    pretty = absurd
