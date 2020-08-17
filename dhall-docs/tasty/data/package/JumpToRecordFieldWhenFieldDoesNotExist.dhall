{-|
On this example, `dhall-docs` will detect that the variable `a` and `b` is of
record-type. If on a selector-expression the field is not found on the type,
no-link will be created

-}
let a = { x = "foo" }
let b = False

in a.y + b.`does not exist`
