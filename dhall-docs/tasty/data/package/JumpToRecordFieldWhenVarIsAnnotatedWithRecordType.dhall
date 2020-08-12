{-|
On this example, `dhall-docs` will detect that the variable `a` is annotated
with a record-type, and associate all jump-to-definition features on its field
access to jump to the fields on that record-type

Note that the `x` field on the record literal isn't highlighted since we consider
that the variable was _defined_ in the annotation
-}
let a : { x : Text } = { x = "foo" }

in a.x
