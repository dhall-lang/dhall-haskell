
module Dhall.Elaboration where

import {-# SOURCE #-} Dhall.Core
import Dhall.Eval (Raw, Core, Val)
import Dhall.Context (Cxt, ElabM)

infer :: Cxt -> Raw -> ElabM (Core, Val)

check ::
     Cxt
  -> Raw
  -> Val
  -> Maybe (Core -> Val -> ElabM X)
  -> ElabM Core