{-|
On this example, `dhall-docs` will detect that the variable `a` is bounded
to a record literal. It will also be aware that that the attribute `x`. When
the user clicks the `x` from `a.x`, it will jump to the `x` punned-entry
-}
let x = 1
let a = { x }

in a.x
