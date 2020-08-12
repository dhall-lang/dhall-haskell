{-|
On this example, `dhall-docs` will detect that the variable `a` and `b` is of
record-type. If on a field-access expression the field is not found on the type,
no-link will be created

-}
let a = { x = "foo" }
let b : { x : Text }= { x = "bar" }

in a.y
