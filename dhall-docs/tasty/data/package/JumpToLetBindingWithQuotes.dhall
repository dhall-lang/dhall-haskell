{-|
This test cases shows that the feature works with quoted variables
-}
let `quoted variable` = 1

let nonQuoted = 2

in `quoted variable` + `nonQuoted`
