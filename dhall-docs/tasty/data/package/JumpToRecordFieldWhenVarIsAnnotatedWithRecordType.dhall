{-|
On this example, even when the variable `a` is annotated with a record-type,
`dhall-docs` will infer the actual type from the bounded value.

All interactions with fields on selector-expressions won't affect the rendered-type.

(The example is intended to not type-check as a Dhall expression)
-}
let a : { x : Text } = { x = "foo", y = "bar" }

in a.x ++ a.y
