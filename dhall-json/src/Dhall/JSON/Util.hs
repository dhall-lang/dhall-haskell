{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Dhall.JSON.Util
    ( pattern V
    , pattern FA
    ) where

import Data.Text  (Text)
import Dhall.Core (Expr, FieldAccess)

import qualified Dhall.Core as Core

pattern V :: Int -> Expr s a
pattern V n = Core.Var (Core.V "_" n)

pattern FA :: Text -> FieldAccess s
pattern FA t <- Core.FieldAccess _ t _
  where FA = Core.makeFieldAccess
