{-|
On this example, `dhall-docs` will detect that the variable `a` is bounded
to a record-literal. When the user clicks on the `field` in the `a.field`, it
will jump to the definition of that field in the source code i.e. to the `field`
key in the record literal
-}
let a = { field = "foo" }

in Text/show a.field
