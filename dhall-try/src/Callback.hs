{-# LANGUAGE JavaScriptFFI #-}

-- | Minimal GHC 9.6 stand-in for @GHC.JS.Foreign.Callback@ (added in GHC 9.8).
-- Uses the same RTS helpers (@h$makeCallback@ / @h$run@) that 9.8's @base@
-- module calls.
module Callback
    ( Callback
    , asyncCallback
    ) where

import GHC.Exts         (Any)
import GHC.JS.Prim      (JSVal)
import Unsafe.Coerce    (unsafeCoerce)

newtype Callback a = Callback JSVal

asyncCallback :: IO () -> IO (Callback (IO ()))
asyncCallback action = js_asyncCallback (unsafeCoerce action)

foreign import javascript unsafe
  "(($1) => { return h$makeCallback(h$run, [], $1); })"
  js_asyncCallback :: Any -> IO (Callback (IO b))
