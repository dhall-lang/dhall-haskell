{-|
On this example, `dhall-docs` will detect that the variable `a` is bounded
to a record literal. It will also detect that the `field` key from the record
literal is another record literal. When the user clicks `deep` from the
`a.field.deep`, it will jump to the definition of that variable in the source
code.
-}
let a = { field = { deep = "bar" } }

in Text/show a.field.deep
