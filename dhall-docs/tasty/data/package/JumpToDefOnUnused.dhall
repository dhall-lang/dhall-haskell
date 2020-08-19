{-|
`dhall-docs` will detect some names (record fields, let-bindings and lam-bindings)
are unused and therefore won't be highlighted
-}

let unused = 1
let used = 3

let unusedRecord = { foo = 1 }
let usedRecord = { foo = 1 }

let f =
       \(unused : Bool)
    -> \(used : Natural)
    -> \(r : { unused : Text, used : Natural}) -> used + r.used

in used + usedRecord.foo + f True 1 { unused = "foo", used = 3 }
