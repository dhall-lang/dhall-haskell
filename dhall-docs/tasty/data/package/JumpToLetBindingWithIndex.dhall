{-|
Although we have a similar example on this [file](./RenderTypeIndexesExample.dhall),
this example showcases the jump-to-definition feature when indexes are used.
-}
let a = 1

let a = 2

in a@1
