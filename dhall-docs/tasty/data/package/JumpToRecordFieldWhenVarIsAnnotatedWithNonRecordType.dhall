{-|
On this example, `dhall-docs` will detect that the variable `a` is annotated
with a non-record type, therefore not associating any information for jump to
definition i.e. hovering the `x` on the field-access expression won't highlight
anything
-}
let a : Bool = { x = "foo" }

in a.x
