{-|
On this example, `dhall-docs` will detect that the variable `a` is annotated
with a type, but it will ignore it anyways. On let-bindings, `dhall-docs` infers
the type of the variable from the bounded expression. As a consequence, on
this example, hovering the `x` on the selector-expression will highlight the
`x` label on the record-literal
-}
let a : Bool = { x = "foo" }

in a.x
