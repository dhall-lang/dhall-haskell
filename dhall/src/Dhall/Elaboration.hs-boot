
module Dhall.Elaboration where

import Dhall.Eval (Raw, Core, Val)
import Dhall.Context (Cxt, ElabM)

data TypeError

infer :: Cxt -> Raw -> ElabM (Core, Val)

check ::
     Cxt
  -> Raw
  -> Val
  -> Maybe (Core -> Val -> TypeError)
  -> ElabM Core