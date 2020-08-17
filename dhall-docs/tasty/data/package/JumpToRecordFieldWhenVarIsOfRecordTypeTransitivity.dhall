{-|
On this example, `dhall-docs` will detect that the variable `b` is bounded
to the value of `a`, and the later is bounded to a record-literal. All selector
expressions over `b` will interact with the record-literal assigned to `a`, since
that is the actual definition of the fields.
-}
let a = { x = 1 }
let b = a

in a.x + b.x
