{-|
On this example, `dhall-docs` will detect that the variable `a` is bounded
to a record literal and it will be aware that it uses dot-syntax.

Transitivity is respected when the variable is assigned using let-bindings.
For instance, if the user clicks the `y` from the `ax.y.z`, it will jump to
the definition of `x` on the record literal.

_(the code is the same from [](./JumpToRecordFieldWhenVarIsOfRecordTypeWithDotSyntax.dhall))_
-}
let a = { x.y.z = 1 }

let ax = a.x
let axy = a.x.y
let axyz = a.x.y.z

in ax.y.z + axy.z + axyz
