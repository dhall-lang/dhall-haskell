{-|
This example shows that `dhall-docs` will find render the correct type on the
index respecting variable indices
-}

let x : Bool = True
let x : Natural = 1

in x@1
