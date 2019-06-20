{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Dhall.JSON.Util
    ( pattern V
    ) where

import Dhall.Core (Expr)

import qualified Dhall.Core as Core

pattern V :: Integer -> Expr s a
pattern V n = Core.Var (Core.V "_" n)
