{-|
On this example, `dhall-docs` will detect that the variable `a` is bounded
to a record literal and it will be aware that it uses dot-syntax.

When the user clicks `x` from `a.x`, it will jump to the `x` on the record-literal.
The same applies for each other field.
-}
let a = { x.y.z = 1 }

let ax = a.x
let axy = a.x.y
let axyz = a.x.y.z

in ax.y.z + axy.z + axyz
