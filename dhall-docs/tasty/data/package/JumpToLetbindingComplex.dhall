{-|
A complex example of jump-to-definition on let-bindings
-}
let f = \(f : Natural) ->
    let a = 1 in f + a

let a = f 1

let b = a + f 3

in b
